import yaml
import os

def list_to_prolog_list(lst):
    return '[' + ', '.join(lst) + ']'

def generate_prolog_rule(rule):
    name = rule['name']
    priority = rule['priority']
    event_types = rule.get('event_type', [])
    conditions = rule.get('conditions', [])
    actions = rule.get('actions', [])

    patterns_str = list_to_prolog_list(event_types)
    conditions_str = list_to_prolog_list(conditions)
    actions_str = list_to_prolog_list(actions)

    return (
        f"refine_rule(\n"
        f"    {name},\n"
        f"    {priority},\n"
        f"    {patterns_str},\n"
        f"    {conditions_str},\n"
        f"    {actions_str}\n"
        f").\n"
    )

def convert_yaml_to_prolog(yaml_path, prolog_path):
    with open(yaml_path, 'r') as f:
        data = yaml.safe_load(f)

    rules = data.get('refine_rules', [])
    with open(prolog_path, 'w') as f:
        for rule in rules:
            f.write(generate_prolog_rule(rule))
            f.write('\n')

def convert_all_yaml_to_prolog():
    yaml_dir = "../yaml"
    prolog_dir = "../rules"
    for filename in os.listdir(yaml_dir):
        if filename.endswith((".yaml", ".yml")):
            yaml_path = os.path.join(yaml_dir, filename)
            if "refine" in filename:
                prolog_filename = "refine.pl"
                prolog_path = os.path.join(prolog_dir, prolog_filename)
                print(f"Converting {yaml_path} to {prolog_path}")
                convert_yaml_to_prolog(yaml_path, prolog_path)

if __name__ == "__main__":
    convert_all_yaml_to_prolog()

