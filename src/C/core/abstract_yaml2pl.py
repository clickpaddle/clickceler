import yaml
from jinja2 import Environment, FileSystemLoader
import os

def convert_all_yaml_to_prolog():
    base_dir = os.path.dirname(os.path.abspath(__file__))
    yaml_file = os.path.join(base_dir, "../yaml/abstract.yaml")
    template_dir = os.path.join(base_dir, "./templates")
    output_file = os.path.join(base_dir, "../rules/abstract.pl")

    # Load YAML
    with open(yaml_file, "r") as f:
        data = yaml.safe_load(f)

    # Jinja2 environment
    env = Environment(loader=FileSystemLoader(template_dir))
    template = env.get_template("abstract.j2")

    # Render template
    prolog_code = template.render(abstract_rules=data["abstract_rules"])

    # Write output file
    with open(output_file, "w") as f:
        f.write(prolog_code)

    print(f"✅ abstract rules generated in {output_file}")

if __name__ == "__main__":
    convert_all_yaml_to_prolog()

