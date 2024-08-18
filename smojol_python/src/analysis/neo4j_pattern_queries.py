from typing import LiteralString


def call_sentence_pattern() -> LiteralString:
    return """
            MATCH path=(start:SENTENCE)-[:FOLLOWED_BY*]->(end:SENTENCE)
            WHERE ALL(n IN nodes(path)[0..-1] WHERE n:SENTENCE AND (n)-[:STARTS_WITH]->(:MOVE)) AND (end)-[:STARTS_WITH]->(:CALL)
            RETURN path
            """


def call_statement_pattern() -> LiteralString:
    return """
            MATCH path=(start:MOVE)-[:FOLLOWED_BY*]->(end:CALL)
            WHERE ALL(n IN nodes(path)[0..-1] WHERE n:MOVE)
            RETURN path
            """
