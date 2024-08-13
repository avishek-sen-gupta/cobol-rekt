from __future__ import annotations


class DataNode:
    def __init__(self, data_id: str, name: str, content: str, level_number: str, raw_text: str,
                 is_redefinition: bool, redefines: str):
        self.id = data_id
        self.name = name
        self.content = content
        self.level_number = level_number
        self.raw_text = raw_text
        self.is_redefinition = is_redefinition
        self.redefines = redefines

    @classmethod
    def from_dict(cls, v):
        return cls(v["id"], v["name"], v["content"], v["levelNumber"], v["rawText"],
                   v["isRedefinition"], v["redefines"])
