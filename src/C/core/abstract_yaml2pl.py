import yaml
from jinja2 import Environment, FileSystemLoader
import os
import glob

def convert_all_yaml_to_prolog():
    """
    Explore tous les fichiers YAML dans un répertoire et génère un fichier Prolog
    unique avec toutes les règles abstract_rule.
    
    Les chemins des fichiers et répertoires sont définis en dur ici.
    """
    # Paramètres définis statiquement
    yaml_dir = "../yaml"                       # Répertoire contenant tous les YAML
    template_file = "./templates/abstract.j2"      # Template Jinja2
    output_file = "../rules/abstract.pl"     # Fichier Prolog de sortie

    # Vérifications
    if not os.path.isdir(yaml_dir):
        raise FileNotFoundError(f"Répertoire YAML introuvable : {yaml_dir}")
    if not os.path.isfile(template_file):
        raise FileNotFoundError(f"Template Jinja2 introuvable : {template_file}")

    all_rules = []

    # Parcours tous les fichiers YAML du répertoire
    for yaml_path in glob.glob(os.path.join(yaml_dir, "*.yaml")):
        with open(yaml_path, "r") as f:
            data = yaml.safe_load(f)
            if "abstract_rules" in data:
                all_rules.extend(data["abstract_rules"])

    if not all_rules:
        print("⚠️ Aucun abstract_rule trouvé dans le répertoire.")
        return

    # Configurer Jinja2
    env = Environment(loader=FileSystemLoader(os.path.dirname(template_file) or "."))
    template = env.get_template(os.path.basename(template_file))

    # Générer le texte Prolog
    output = template.render(abstract_rules=all_rules)

    # Sauvegarder dans un fichier .pl
    os.makedirs(os.path.dirname(output_file), exist_ok=True)
    with open(output_file, "w") as f:
        f.write(output)

    print(f"✅ Toutes les règles Prolog générées dans {output_file}")


# Main block réduit
if __name__ == "__main__":
    convert_all_yaml_to_prolog()

