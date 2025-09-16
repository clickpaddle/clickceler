#!/usr/bin/env python3
import yaml
from jinja2 import Environment, FileSystemLoader
from pathlib import Path

def convert_all_yaml_to_prolog():
    yaml_dir = Path('../yaml')       # dossier où se trouvent tes YAML
    output_file = Path('../rules/abstract_rules.pl')  # fichier Prolog de sortie
    template_dir = Path('./templates')  # dossier contenant le template Jinja2

    all_rules = []

    # Charger tous les fichiers YAML
    for yaml_file in yaml_dir.glob('*.yaml'):
        with open(yaml_file, 'r') as f:
            data = yaml.safe_load(f)
            if 'abstract_rules' in data:
                all_rules.extend(data['abstract_rules'])

    # Charger le template Jinja2
    env = Environment(loader=FileSystemLoader(template_dir), trim_blocks=True, lstrip_blocks=True)
    template = env.get_template('abstract.j2')

    # Générer le Prolog
    output = template.render(abstract_rules=all_rules)

    # Créer le dossier si nécessaire et écrire dans le fichier
    output_file.parent.mkdir(parents=True, exist_ok=True)
    with open(output_file, 'w') as f:
        f.write(output)

    print(f"✅ Prolog rules generated in {output_file}")

if __name__ == '__main__':
    convert_all_yaml_to_prolog()

