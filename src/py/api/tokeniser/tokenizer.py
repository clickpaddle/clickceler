import re

def tokenize(query):
    token_pattern = r'''("[^"]*"|'[^']*'|~=|\w+|>=|<=|>|<|=|,)'''
    tokens = re.findall(token_pattern, query)
    return tokens

def parse(tokens):
    if tokens[0].upper() != "SELECT":
        raise ValueError("Only SELECT supported")
    select_field = tokens[1]
    if tokens[2].upper() != "FROM":
        raise ValueError("Missing FROM")
    from_table = tokens[3]
    where_clause = None
    if len(tokens) > 4 and tokens[4].upper() == "WHERE":
        where_field = tokens[5]
        operator = tokens[6]
        value = tokens[7]
        # On retire les guillemets si la valeur est une chaîne
        if (value.startswith('"') and value.endswith('"')) or (value.startswith("'") and value.endswith("'")):
            value = value[1:-1]
        where_clause = (where_field, operator, value)
    return {
        "select": select_field,
        "from": from_table,
        "where": where_clause
    }

query = "SELECT name FROM users WHERE city ~= 'Paris'"
tokens = tokenize(query)
ast = parse(tokens)
print("Tokens:", tokens)
print("AST:", ast)

