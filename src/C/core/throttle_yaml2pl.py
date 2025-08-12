import yaml
import os

def list_to_prolog_list(lst):
    """Converts a Python list to a Prolog list, quoting simple strings."""
    def quote_if_str(s):
        if isinstance(s, str):
            s = s.strip()
            # Check if the string is already a Prolog term (e.g., 'term', "term", [list], {dict}, etc.)
            # This is a more robust check to avoid double-quoting.
            if not (s.startswith('[') or s.startswith('{') or s.startswith('(') or s.startswith("'") or s.startswith('"')):
                return f"'{s}'"
            return s
        return str(s)

    # Ensure the input is a list, defaulting to an empty list if None
    if lst is None:
        return '[]'
    
    return '[' + ', '.join(quote_if_str(e) for e in lst) + ']'

def generate_prolog_rule(rule):
    """Generates a throttle_rule/6 Prolog fact from a rule dictionary."""
    # Use .get() to provide a default value (e.g., empty list) for optional keys
    name = rule.get('name')
    priority = rule.get('priority', 0)
    event_types = rule.get('event_type', [])
    conditions = rule.get('conditions', [])
    settings = rule.get('settings', [])
    # Tolérance for 'action' or 'actions'
    actions = rule.get('actions', rule.get('action', []))

    # All these fields are required for a valid rule
    if not name:
        raise ValueError("Rule is missing a 'name' field.")

    event_types_str = list_to_prolog_list(event_types)
    conditions_str = list_to_prolog_list(conditions)
    
    # Handle the 'settings' list which might contain dictionaries
    # The original script's logic for quoting here was a bit simplistic.
    # This version directly converts the Python dict to a Prolog dict string.
    settings_prolog = []
    for setting in settings:
        if isinstance(setting, dict):
            # Format the dictionary into a Prolog-friendly string
            dict_items = [f"'{k}': '{v}'" for k, v in setting.items()]
            settings_prolog.append(f"settings{{{', '.join(dict_items)}}}")
        else:
            settings_prolog.append(str(setting))
    settings_str = '[' + ', '.join(settings_prolog) + ']'

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
    """Converts a single YAML file to a Prolog file."""
    try:
        with open(yaml_path, 'r', encoding='utf-8') as f:
            data = yaml.safe_load(f)
    except FileNotFoundError:
        print(f"Error: YAML file not found at {yaml_path}")
        return
    except yaml.YAMLError as e:
        print(f"Error parsing YAML file {yaml_path}: {e}")
        return

    rules = data.get('throttle_rules', [])
    
    # Use a temporary list to collect all rules before writing
    prolog_rules = []
    for rule in rules:
        try:
            prolog_rules.append(generate_prolog_rule(rule))
        except ValueError as e:
            print(f"Error generating rule from {yaml_path}: {e}")

    if not prolog_rules:
        print(f"No valid throttle rules found in {yaml_path}")
        return

    try:
        with open(prolog_path, 'w', encoding='utf-8') as f:
            f.write('% Auto-generated throttle rules from YAML\n\n')
            f.write(''.join(prolog_rules))
        print(f"Successfully converted rules to {prolog_path}")
    except IOError as e:
        print(f"Error writing to Prolog file {prolog_path}: {e}")

def convert_all_yaml_to_prolog():
    """Scans for and converts all relevant YAML files."""
    yaml_dir = "../yaml"
    prolog_dir = "../rules"
    
    # Use a more flexible path handling
    base_dir = os.path.dirname(os.path.abspath(__file__))
    yaml_dir_path = os.path.join(base_dir, yaml_dir)
    prolog_dir_path = os.path.join(base_dir, prolog_dir)

    # Create target directories if they don't exist
    os.makedirs(prolog_dir_path, exist_ok=True)
    
    # Flag to check if any file was processed
    found_yaml = False

    if not os.path.isdir(yaml_dir_path):
        print(f"Error: YAML directory not found at {yaml_dir_path}")
        return

    for filename in os.listdir(yaml_dir_path):
        if filename.endswith((".yaml", ".yml")) and "throttle" in filename:
            yaml_path = os.path.join(yaml_dir_path, filename)
            prolog_path = os.path.join(prolog_dir_path, "throttle.pl")
            print(f"Converting {yaml_path} to {prolog_path}")
            convert_yaml_to_prolog(yaml_path, prolog_path)
            found_yaml = True
    
    if not found_yaml:
        print(f"No YAML files containing 'throttle' were found in {yaml_dir_path}")

if __name__ == "__main__":
    convert_all_yaml_to_prolog()
