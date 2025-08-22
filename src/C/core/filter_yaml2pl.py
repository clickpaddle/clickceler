#!/usr/bin/env python3
import yaml
from jinja2 import Environment, FileSystemLoader
from pathlib import Path

def convert_all_yaml_to_prolog():
    yaml_dir = Path('../yaml')
    output_file = Path('../rules/filter.pl')
    template_dir = Path(__file__).parent / 'templates'

    all_rules = []

    # Charger tous les fichiers YAML
    for yaml_file in yaml_dir.glob('*.yaml'):
        with open(yaml_file, 'r') as f:
            data = yaml.safe_load(f)
            if 'filter_rules' in data:
                all_rules.extend(data['filter_rules'])

    # Charger le template Jinja2
    env = Environment(loader=FileSystemLoader(template_dir))
    template = env.get_template('filter.j2')

    # Générer le Prolog
    output = template.render(filter_rules=all_rules)

    # Écrire dans le fichier
    output_file.parent.mkdir(parents=True, exist_ok=True)
    with open(output_file, 'w') as f:
        f.write(output)

if __name__ == '__main__':
    convert_all_yaml_to_prolog()

