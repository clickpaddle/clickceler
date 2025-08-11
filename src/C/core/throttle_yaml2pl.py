import yaml
import os

def list_to_prolog_list(lst):
    # Entoure chaque élément de quotes s'il s'agit d'une string simple (non déjà Prolog)
    def quote_if_str(s):
        if isinstance(s, str):
            s = s.strip()
            if not (s.startswith('[') or s.startswith('(') or s.startswith("'") or s.startswith('"')):
                return f"'{s}'"
            return s
        return str(s)

    return '[' + ', '.join(quote_if_str(e) for e in lst) + ']'

def generate_prolog_rule(rule):
    name = rule['name']
    priority = rule['priority']
    event_types = rule.get('event_type', [])
    conditions = rule.get('conditions', [])
    settings = rule.get('settings', [])
    actions = rule.get('actions', []) or rule.get('action', [])  # tolérance 'action' ou 'actions'

    event_types_str = list_to_prolog_list(event_types)
    conditions_str = list_to_prolog_list(conditions)
    settings_str = list_to_prolog_list(settings)
    actions_str = list_to_prolog_list(actions)

    return (
        f"throttle_rule(\n"
        f"    {name},\n"
        f"    {priority},\n"
        f"    {event_types_str},\n"
        f"    {conditions_str},\n"
        f"    {settings_str},\n"
        f"    {actions_str}\n"
        f").\n"
    )

def convert_yaml_to_prolog(yaml_path, prolog_path):
    with open(yaml_path, 'r') as f:
        data = yaml.safe_load(f)

    rules = data.get('throttle_rules', [])

    with open(prolog_path, 'w') as f:
        f.write('% Auto-generated throttle rules\n\n')
        for rule in rules:
            f.write(generate_prolog_rule(rule))
            f.write('\n')

def convert_all_yaml_to_prolog():
    yaml_dir = "../yaml"
    prolog_dir = "../rules"
    for filename in os.listdir(yaml_dir):
        if filename.endswith((".yaml", ".yml")):
            if "throttle" in filename:
                yaml_path = os.path.join(yaml_dir, filename)
                prolog_path = os.path.join(prolog_dir, "throttle.pl")
                print(f"Converting {yaml_path} to {prolog_path}")
                convert_yaml_to_prolog(yaml_path, prolog_path)

if __name__ == "__main__":
    convert_all_yaml_to_prolog()

