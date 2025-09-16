import os
import re
import random
import string
import json

# === Random name generator ===
def random_name(length=6, upper=False):
    letters = string.ascii_uppercase if upper else string.ascii_lowercase
    return ''.join(random.choice(letters) for _ in range(length))

# === Global dictionaries ===
pred_map = {}  # (name, arity) -> obfuscated_name
var_map = {}   # variable -> obfuscated_name

# Built-in predicates to ignore (extend this if needed)
BUILTINS = {"member", "write", "read", "assertz", "retract", "is", "not", "true", "fail"}

# === Predicate detection ===
def find_predicates(code):
    preds = set()
    # Match "name(args) :-" or "name(args)."
    pattern = re.compile(r'([a-z_][a-zA-Z0-9_]*)\s*\((.*?)\)\s*[:-\.]')
    for match in pattern.finditer(code):
        name = match.group(1)
        args = match.group(2).split(",")
        arity = len(args)
        if name not in BUILTINS:
            preds.add((name, arity))
    return preds

# === Predicate replacement ===
def replace_predicates(code):
    for (name, arity), new_name in pred_map.items():
        regex = re.compile(r'\b' + re.escape(name) + r'\s*(?=\()')
        code = regex.sub(new_name, code)
    return code

# === Variable obfuscation with global dictionary ===
def obfuscate_variables(clause):
    # Variables = tokens starting with uppercase or "_"
    variables = re.findall(r'\b[A-Z_][A-Za-z0-9_]*\b', clause)
    for var in set(variables):
        if var not in var_map:
            var_map[var] = random_name(12, upper=True)  # 12-char uppercase
    for old, new in var_map.items():
        clause = re.sub(r'\b' + re.escape(old) + r'\b', new, clause)
    return clause

# === File processor ===
def process_file(filepath):
    with open(filepath, "r", encoding="utf-8") as f:
        code = f.read()

    # Detect predicates and assign random names if needed
    preds = find_predicates(code)
    for pred in preds:
        if pred not in pred_map:
            pred_map[pred] = random_name(6, upper=False)

    # Replace predicates
    code = replace_predicates(code)

    # Replace variables (global dictionary applied clause by clause)
    new_clauses = []
    for clause in code.split('.'):
        if clause.strip():
            new_clauses.append(obfuscate_variables(clause))
    new_code = '.\n'.join(new_clauses) + '.\n'

    with open(filepath, "w", encoding="utf-8") as f:
        f.write(new_code)

# === Repo processor ===
def process_repo(repo_path):
    for root, _, files in os.walk(repo_path):
        for file in files:
            if file.endswith(".pl"):
                process_file(os.path.join(root, file))

if __name__ == "__main__":
    repo_path = "./mon_projet_prolog"  # change this to your repo path
    process_repo(repo_path)

    # Save mapping to JSON
    mapping = {"predicates": pred_map, "variables": var_map}
    with open("obfuscation_map.json", "w", encoding="utf-8") as f:
        json.dump(mapping, f, indent=4)

    print("✅ Obfuscation done. Mapping saved to obfuscation_map.json")
import os
import re
import random
import string
import json

# === Random name generator ===
def random_name(length=6, upper=False):
    letters = string.ascii_uppercase if upper else string.ascii_lowercase
    return ''.join(random.choice(letters) for _ in range(length))

# === Global dictionaries ===
pred_map = {}  # (name, arity) -> obfuscated_name
var_map = {}   # variable -> obfuscated_name

# Built-in predicates to ignore (extend this if needed)
BUILTINS = {"member", "write", "read", "assertz", "retract", "is", "not", "true", "fail"}

# === Predicate detection ===
def find_predicates(code):
    preds = set()
    # Match "name(args) :-" or "name(args)."
    pattern = re.compile(r'([a-z_][a-zA-Z0-9_]*)\s*\((.*?)\)\s*[:-\.]')
    for match in pattern.finditer(code):
        name = match.group(1)
        args = match.group(2).split(",")
        arity = len(args)
        if name not in BUILTINS:
            preds.add((name, arity))
    return preds

# === Predicate replacement ===
def replace_predicates(code):
    for (name, arity), new_name in pred_map.items():
        regex = re.compile(r'\b' + re.escape(name) + r'\s*(?=\()')
        code = regex.sub(new_name, code)
    return code

# === Variable obfuscation with global dictionary ===
def obfuscate_variables(clause):
    # Variables = tokens starting with uppercase or "_"
    variables = re.findall(r'\b[A-Z_][A-Za-z0-9_]*\b', clause)
    for var in set(variables):
        if var not in var_map:
            var_map[var] = random_name(12, upper=True)  # 12-char uppercase
    for old, new in var_map.items():
        clause = re.sub(r'\b' + re.escape(old) + r'\b', new, clause)
    return clause

# === File processor ===
def process_file(filepath):
    with open(filepath, "r", encoding="utf-8") as f:
        code = f.read()

    # Detect predicates and assign random names if needed
    preds = find_predicates(code)
    for pred in preds:
        if pred not in pred_map:
            pred_map[pred] = random_name(6, upper=False)

    # Replace predicates
    code = replace_predicates(code)

    # Replace variables (global dictionary applied clause by clause)
    new_clauses = []
    for clause in code.split('.'):
        if clause.strip():
            new_clauses.append(obfuscate_variables(clause))
    new_code = '.\n'.join(new_clauses) + '.\n'

    with open(filepath, "w", encoding="utf-8") as f:
        f.write(new_code)

# === Repo processor ===
def process_repo(repo_path):
    for root, _, files in os.walk(repo_path):
        for file in files:
            if file.endswith(".pl"):
                process_file(os.path.join(root, file))

if __name__ == "__main__":
    repo_path = "./mon_projet_prolog"  # change this to your repo path
    process_repo(repo_path)

    # Save mapping to JSON
    mapping = {"predicates": pred_map, "variables": var_map}
    with open("obfuscation_map.json", "w", encoding="utf-8") as f:
        json.dump(mapping, f, indent=4)

    print("â�� Obfuscation done. Mapping saved to obfuscation_map.json")

