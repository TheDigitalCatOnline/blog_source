class MicroScheduler(object):
    def __init__(self):
        self.active_microthreads = []
        self.scheduled_microthreads = []

    def add_microthread(self, mthread):
        g = mthread.main()
        g.next()
        self.active_microthreads.append(g)

    def main(self):
        yield 1
        while 1:
            if len(self.active_microthreads) == 0:
                yield 1
            for thread in self.active_microthreads:
                try:
                    thread.next()
                    self.scheduled_microthreads.append(thread)
                except StopIteration:
                    pass
                yield 1
        
            self.active_microthreads = self.scheduled_microthreads
            self.scheduled_microthreads = []
