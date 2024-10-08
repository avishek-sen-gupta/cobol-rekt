class ConsoleColors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    GREY = '\033[90m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

    def grey(self, message: str) -> str:
        return f"{self.OKBLUE}{message}{self.ENDC}"

    def blue(self, message: str) -> str:
        return f"{self.OKBLUE}{message}{self.ENDC}"

    def cyan(self, message: str) -> str:
        return f"{self.OKCYAN}{message}{self.ENDC}"

    def green(self, message: str) -> str:
        return f"{self.OKGREEN}{message}{self.ENDC}"

    def bold(self, message: str) -> str:
        return f"{self.BOLD}{message}{self.ENDC}"

    def underline(self, message: str) -> str:
        return f"{self.UNDERLINE}{message}{self.ENDC}"

    def warning(self, message: str) -> str:
        return f"{self.WARNING}{message}{self.ENDC}"

    def fail(self, message: str) -> str:
        return f"{self.FAIL}{message}{self.ENDC}"
