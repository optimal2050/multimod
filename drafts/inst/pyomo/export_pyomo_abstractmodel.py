#!/usr/bin/env python3
"""Export a Pyomo AbstractModel declaration to a JSON structure consumable by multimod.

This is intentionally declaration-first:
- Works without instantiating the model (no data required)
- Exports component *signatures* (names, index sets, domains, defaults)
- Constraint/objective algebra is not exported at this stage

A future extension can add an "instance" mode that exports expression trees.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import os
import sys
import inspect
from typing import Any, Dict, List, Optional, Tuple


def _json_sanitize(obj: Any) -> Any:
    """Recursively convert objects to JSON-serializable primitives.

    Pyomo declarations can include sentinel objects and classes/types.
    For a declaration-first export we keep these as strings.
    """

    if obj is None or isinstance(obj, (str, int, float, bool)):
        return obj

    if isinstance(obj, type):
        return obj.__name__

    if isinstance(obj, dict):
        return {str(k): _json_sanitize(v) for k, v in obj.items()}

    if isinstance(obj, (list, tuple)):
        return [_json_sanitize(v) for v in obj]

    # Last resort: string representation
    try:
        return str(obj)
    except Exception:
        return repr(obj)


def _safe_callable_name(obj: Any) -> Optional[str]:
    try:
        name = getattr(obj, "__name__", None)
        if isinstance(name, str) and name:
            return name
    except Exception:
        pass
    return None


def _safe_callable_module(obj: Any) -> Optional[str]:
    try:
        mod = getattr(obj, "__module__", None)
        if isinstance(mod, str) and mod:
            return mod
    except Exception:
        pass
    return None


def _try_get_source(obj: Any) -> Optional[str]:
    try:
        if obj is None or not callable(obj):
            return None
        src = inspect.getsource(obj)
        if isinstance(src, str) and src.strip():
            return src
    except Exception:
        return None
    return None


def _expr_to_str(expr: Any) -> Optional[str]:
    if expr is None:
        return None
    # Prefer Pyomo's expression_to_string if available; fall back to str().
    try:
        from pyomo.core.expr.visitor import expression_to_string  # type: ignore

        return expression_to_string(expr)
    except Exception:
        pass
    try:
        from pyomo.core.expr.current import expression_to_string  # type: ignore

        return expression_to_string(expr)
    except Exception:
        pass
    try:
        return str(expr)
    except Exception:
        return repr(expr)


def _iter_component_data(comp: Any) -> List[Any]:
    # Returns instantiated component data entries (ConstraintData / ObjectiveData)
    try:
        return list(comp.values())
    except Exception:
        return []


def _infer_relation_from_bounds(lower: Any, upper: Any) -> Optional[str]:
    # Pyomo constraints can be: lower <= body <= upper
    # multimod relation supports only one-sided or equality.
    if lower is None and upper is None:
        return None
    if lower is not None and upper is not None:
        # if both bounds exist, treat as equality only when identical after stringification
        if _expr_to_str(lower) == _expr_to_str(upper):
            return "=="
        # ranged constraint; caller must decide how to represent
        return "range"
    if lower is None and upper is not None:
        return "<="
    if lower is not None and upper is None:
        return ">="
    return None


def _load_module_from_path(path: str):
    spec = importlib.util.spec_from_file_location("_multimod_pyomo_source", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Unable to import module from path: {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _safe_name(obj: Any) -> Optional[str]:
    name = getattr(obj, "name", None)
    if isinstance(name, str) and name:
        return name
    return None


def _doc(obj: Any) -> Optional[str]:
    doc = getattr(obj, "doc", None)
    if isinstance(doc, str) and doc:
        return doc
    return None


def _as_bool(x: Any) -> Optional[bool]:
    if isinstance(x, bool):
        return x
    return None


def _index_dim_names(index_set: Any) -> List[str]:
    # Best-effort conversion of an index set (possibly SetProduct) to dimension set names.
    if index_set is None:
        return []

    # Scalar components use UnindexedComponent_set
    if _safe_name(index_set) in ("UnindexedComponent_set", None) and str(index_set) == "UnindexedComponent_set":
        return []

    # Newer Pyomo uses .subsets() for SetProduct-like objects
    try:
        subsets = list(index_set.subsets())  # type: ignore[attr-defined]
        dims: List[str] = []
        for s in subsets:
            n = _safe_name(s)
            dims.append(n if n is not None else str(s))
        # If we got something meaningful, return it
        if any(dims):
            return dims
    except Exception:
        pass

    # Fallback: try set_tuple (older patterns)
    try:
        tup = getattr(index_set, "set_tuple")
        dims = []
        for s in tup:
            n = _safe_name(s)
            dims.append(n if n is not None else str(s))
        if any(dims):
            return dims
    except Exception:
        pass

    # Last resort: opaque string
    return [str(index_set)]


def _domain_name(obj: Any) -> Optional[str]:
    try:
        dom = getattr(obj, "domain", None)
        n = _safe_name(dom)
        if n is not None:
            return n
        if dom is not None:
            return str(dom)
    except Exception:
        pass
    return None


def _bounds_signature(var: Any) -> Dict[str, Any]:
    # AbstractModel bounds might be rule-based; do not attempt evaluation.
    bounds: Dict[str, Any] = {"lo": None, "up": None, "rule": False}

    # Some Var objects carry a .bounds attribute (tuple) even in AbstractModel
    try:
        b = getattr(var, "bounds", None)
        if callable(b):
            bounds["rule"] = True
            return bounds
        if isinstance(b, tuple) and len(b) == 2:
            bounds["lo"], bounds["up"] = b
            return bounds
    except Exception:
        pass

    # Try explicit lb/ub if available
    for k in ("lb", "ub"):
        try:
            v = getattr(var, k, None)
            if v is not None:
                bounds["lo" if k == "lb" else "up"] = v
        except Exception:
            continue

    return bounds


def export_abstractmodel(
    model: Any,
    name: Optional[str],
    source: str,
    instance: Any = None,
    include_rule_source: bool = False,
) -> Dict[str, Any]:
    try:
        from pyomo.core.base import Constraint, Objective, Param, Set, Var  # type: ignore
    except Exception as e:
        raise RuntimeError(
            "Pyomo is required in the Python environment used for export. "
            "Install pyomo and retry."
        ) from e

    ms: Dict[str, Any] = {
        "schema_version": "0.1",
        "name": name,
        "desc": None,
        "language": "pyomo",
        "source": source,
        "sets": {},
        "mappings": {},
        "aliases": [],
        "parameters": {},
        "variables": {},
        "equations": {},
        "objectives": [],
        "models": {"default": []},
        "metadata": {"export": {"mode": "abstractmodel"}},
    }

    # Sets
    for comp in model.component_objects(Set, active=None):  # type: ignore[attr-defined]
        cname = _safe_name(comp) or str(comp)
        ms["sets"][cname] = {
            "name": cname,
            "desc": _doc(comp),
            "subset_of": None,
            "attrs": {
                "dimen": getattr(comp, "dimen", None),
                "ordered": _as_bool(getattr(comp, "ordered", None)),
                "initialize": bool(getattr(comp, "initialize", None)),
            },
        }

    # Params
    for comp in model.component_objects(Param, active=None):  # type: ignore[attr-defined]
        cname = _safe_name(comp) or str(comp)
        dims = _index_dim_names(comp.index_set())  # type: ignore[call-arg]
        ms["parameters"][cname] = {
            "name": cname,
            "desc": _doc(comp),
            "dims": dims,
            "defVal": getattr(comp, "default", None),
            "symbolic": bool(getattr(comp, "is_expression_type", lambda: False)()),
            "formula": None,
            "data": None,
            "attrs": {
                "mutable": _as_bool(getattr(comp, "mutable", None)),
                "within": _domain_name(comp),
            },
        }

    # Vars
    for comp in model.component_objects(Var, active=None):  # type: ignore[attr-defined]
        cname = _safe_name(comp) or str(comp)
        dims = _index_dim_names(comp.index_set())  # type: ignore[call-arg]
        # vtype is multimod-specific; keep Pyomo domain info in attrs.
        ms["variables"][cname] = {
            "name": cname,
            "desc": _doc(comp),
            "dims": dims,
            "domain": None,
            "vtype": None,
            "bounds": _bounds_signature(comp),
            "comment": None,
            "attrs": {
                "domain": _domain_name(comp),
                "is_binary": bool(getattr(comp, "is_binary", lambda: False)()),
                "is_integer": bool(getattr(comp, "is_integer", lambda: False)()),
            },
        }

    # Constraints (signatures only)
    for comp in model.component_objects(Constraint, active=None):  # type: ignore[attr-defined]
        cname = _safe_name(comp) or str(comp)
        dims = _index_dim_names(comp.index_set())  # type: ignore[call-arg]
        rule_obj = getattr(comp, "rule", None)
        rule_source = _try_get_source(rule_obj) if include_rule_source else None

        ms["equations"][cname] = {
            "name": cname,
            "desc": _doc(comp),
            "dims": dims,
            "relation": None,
            "body": None,
            "expr": None,
            "attrs": {
                "has_rule": bool(rule_obj),
                "rule_name": _safe_callable_name(rule_obj),
                "rule_module": _safe_callable_module(rule_obj),
                "rule_source": rule_source,
                "active": _as_bool(getattr(comp, "active", None)),
            },
        }
        ms["models"]["default"].append(cname)

    # Objectives (signatures only)
    for comp in model.component_objects(Objective, active=None):  # type: ignore[attr-defined]
        cname = _safe_name(comp) or str(comp)
        rule_obj = getattr(comp, "rule", None)
        rule_source = _try_get_source(rule_obj) if include_rule_source else None
        # Standardize sense
        sense = None
        try:
            from pyomo.core.base.objective import minimize, maximize  # type: ignore
            if getattr(comp, "sense", None) is minimize:
                sense = "minimize"
            elif getattr(comp, "sense", None) is maximize:
                sense = "maximize"
        except Exception:
            pass

        obj_entry = {
            "variable": cname,
            "sense": sense or "minimize",
            "model": None,
            "nVals": None,
            "value": None,
            "misc": {
                "pyomo_objective": cname,
                "export": "abstractmodel",
                "rule_name": _safe_callable_name(rule_obj),
                "rule_module": _safe_callable_module(rule_obj),
                "rule_source": rule_source,
            },
        }
        ms["objectives"].append(obj_entry)

    # Optional: instance algebra sampling
    if instance is not None:
        ms["metadata"]["export"]["mode"] = "abstractmodel+instance"
        ms["metadata"]["export"]["instance"] = {"sampled": True}

        # Update constraints with a representative instantiated expression (first active entry)
        try:
            inst_constraints = list(instance.component_objects(Constraint, active=None))  # type: ignore[attr-defined]
        except Exception:
            inst_constraints = []

        for comp in inst_constraints:
            cname = _safe_name(comp) or str(comp)
            if cname not in ms["equations"]:
                continue
            data_entries = _iter_component_data(comp)
            n_data = len(data_entries)
            ms["equations"][cname].setdefault("attrs", {})
            ms["equations"][cname]["attrs"]["n_data"] = n_data
            ms["equations"][cname]["attrs"]["indexed"] = n_data > 1

            # pick first active entry (or just first)
            sample = None
            for entry in data_entries:
                try:
                    if getattr(entry, "active", True):
                        sample = entry
                        break
                except Exception:
                    sample = entry
                    break
            if sample is None and n_data > 0:
                sample = data_entries[0]

            if sample is None:
                continue

            try:
                lower = getattr(sample, "lower", None)
                body = getattr(sample, "body", None)
                upper = getattr(sample, "upper", None)
            except Exception:
                lower, body, upper = None, None, None

            rel = _infer_relation_from_bounds(lower, upper)
            expr_pack = {
                "lower": _expr_to_str(lower),
                "body": _expr_to_str(body),
                "upper": _expr_to_str(upper),
                "relation": rel,
                "sample_index": _expr_to_str(getattr(sample, "index", None)),
            }
            ms["equations"][cname]["expr"] = expr_pack

            # Choose a one-sided or equality representation when possible.
            if rel == "==":
                ms["equations"][cname]["relation"] = "=="
                ms["equations"][cname]["body"] = expr_pack["body"]
            elif rel == "<=":
                ms["equations"][cname]["relation"] = "<="
                ms["equations"][cname]["body"] = f"{expr_pack['body']} <= {expr_pack['upper']}"
            elif rel == ">=":
                ms["equations"][cname]["relation"] = ">="
                ms["equations"][cname]["body"] = f"{expr_pack['body']} >= {expr_pack['lower']}"
            else:
                # ranged or unknown; keep as expr_pack only
                ms["equations"][cname]["relation"] = None
                ms["equations"][cname]["body"] = None

        # Update objectives with representative instantiated expression
        try:
            inst_objectives = list(instance.component_objects(Objective, active=None))  # type: ignore[attr-defined]
        except Exception:
            inst_objectives = []

        for comp in inst_objectives:
            cname = _safe_name(comp) or str(comp)
            # Find corresponding objective entry by variable name
            target = None
            for obj in ms.get("objectives", []):
                if obj.get("variable") == cname:
                    target = obj
                    break
            if target is None:
                continue
            data_entries = _iter_component_data(comp)
            n_data = len(data_entries)
            target.setdefault("misc", {})
            target["misc"]["n_data"] = n_data
            sample = data_entries[0] if n_data > 0 else None
            if sample is None:
                continue
            expr = None
            try:
                expr = getattr(sample, "expr", None)
            except Exception:
                expr = None
            target["misc"]["expr"] = _expr_to_str(expr)

    return ms


def main(argv: Optional[List[str]] = None) -> int:
    p = argparse.ArgumentParser(description="Export Pyomo AbstractModel structure to multimod JSON")
    p.add_argument("--source", required=True, help="Path to a Python file that defines a Pyomo model")
    p.add_argument(
        "--model-expr",
        default="model",
        help="Python expression evaluated in the module globals to obtain the model object (default: 'model')",
    )
    p.add_argument("--name", default=None, help="Optional model name override")
    p.add_argument(
        "--instance-expr",
        default=None,
        help=(
            "Optional Python expression evaluated in module globals to obtain an instantiated model "
            "(e.g., 'model.create_instance(data)'). When provided, exporter will sample instantiated "
            "constraint/objective algebra into the JSON (best-effort)."
        ),
    )
    p.add_argument(
        "--include-rule-source",
        action="store_true",
        help="Include inspect.getsource(rule) for constraints/objectives when available.",
    )
    p.add_argument("--out", required=True, help="Output JSON path")

    args = p.parse_args(argv)

    src = os.path.abspath(args.source)
    mod = _load_module_from_path(src)

    try:
        model = eval(args.model_expr, mod.__dict__)
    except Exception as e:
        raise RuntimeError(
            f"Failed to evaluate --model-expr '{args.model_expr}' in module '{src}'."
        ) from e

    instance = None
    if args.instance_expr:
        try:
            instance = eval(args.instance_expr, mod.__dict__)
        except Exception as e:
            raise RuntimeError(
                f"Failed to evaluate --instance-expr '{args.instance_expr}' in module '{src}'."
            ) from e

    ms = export_abstractmodel(
        model=model,
        name=args.name,
        source=src,
        instance=instance,
        include_rule_source=bool(args.include_rule_source),
    )

    ms = _json_sanitize(ms)

    out_dir = os.path.dirname(os.path.abspath(args.out))
    if out_dir and not os.path.exists(out_dir):
        os.makedirs(out_dir, exist_ok=True)

    with open(args.out, "w", encoding="utf-8") as f:
        json.dump(ms, f, indent=2, ensure_ascii=False)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
