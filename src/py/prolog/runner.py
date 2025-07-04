import subprocess

def run_prolog_query(facts_file, rules_file, query):
    prolog_script = f"""
    ['{facts_file}'],
    ['{rules_file}'],
    findall(ID, {query}, Result),
    writeln(Result),
    halt.
    """
    result = subprocess.run(['swipl', '-q'], input=prolog_script.encode(), capture_output=True)
    return result.stdout.decode().strip()

