class MicroThread(object):
    def step(self):
        pass

    def create(self):
        pass

    def main(self):
        self.create()
        yield 1
        while 1:
            self.step()
            yield 1


