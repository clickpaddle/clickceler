#!/usr/bin/python3
import yaml

def format_value(val):
    if isinstance(val, str):
        # Mettre des quotes si nécessaire (espaces, majuscules, caractères spéciaux)
        if not val.isidentifier() or not val.islower():
            val_escaped = val.replace("'", "\\'")
            return f"'{val_escaped}'"
        return val
    elif isinstance(val, bool):
        return 'true' if val else 'false'
    else:
        return str(val)

def dict_to_prolog_term(d):
    # Ex: {'eq': ['status', 'open']} => eq(status, 'open')
    for key, val in d.items():
        if isinstance(val, list) and len(val) == 2:
            field, value = val
            return f"{key}({field}, {format_value(value)})"
    return ""

def generate_multiline_list(terms, indent='     '):
    filtered = [t for t in terms if t]
    if not filtered:
        return "[]"
    body = ',\n'.join(f"{indent}{term}" for term in filtered)
    return f"[\n{body}\n    ]"

def generate_prolog_rule(rule):
    event_type = rule['event_type']
    priority = rule['priority']
    name = rule['name']

    conditions = rule.get('conditions', [])
    transformations = rule.get('transformations', [])

    cond_terms = [dict_to_prolog_term(c) for c in conditions]
    trans_terms = [dict_to_prolog_term(t) for t in transformations]

    cond_block = generate_multiline_list(cond_terms)
    trans_block = generate_multiline_list(trans_terms)

    return (
        f"refine_rule(\n"
        f"    {event_type},\n"
        f"    {priority},\n"
        f"    '{name}',\n"
        f"    {cond_block},\n"
        f"    {trans_block}\n"
        f")."
    )

def yaml_to_prolog(yaml_file):
    with open(yaml_file, 'r') as f:
        data = yaml.safe_load(f)

    prolog_rules = []
    for item in data:
        rule = item.get('refine_rule')
        if rule:
            prolog_rules.append(generate_prolog_rule(rule))
    return prolog_rules

if __name__ == "__main__":
    input_yaml = "../yaml/refine.yaml"
    output_prolog = "../rules/refine.pl"

    rules = yaml_to_prolog(input_yaml)
    with open(output_prolog, 'w') as f:
        for rule in rules:
            f.write(rule + "\n\n")

    print(f"✅ {len(rules)} rules written to {output_prolog}")

