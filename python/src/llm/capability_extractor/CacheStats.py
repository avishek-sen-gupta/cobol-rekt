class CacheStats:
    hits = 0

    def add(self):
        self.hits += 1
        return self
