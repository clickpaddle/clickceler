from pyswip import Prolog

def execute_prolog_query(query_str):
    prolog = Prolog()
    # Ajout d’un fait simple pour test
    prolog.assertz("parent(pam, bob)")
    prolog.assertz("parent(tom, bob)")

    try:
        q = prolog.query(query_str)
        results = [sol for sol in q]
        q.close()
        return True, results
    except Exception as e:
        return False, f"Erreur Prolog ou autre: {str(e)}"

# Test
success, results = execute_prolog_query("parent(X, bob).")

if success:
    print("Résultats:", results)
else:
    print("Erreur:", results)
