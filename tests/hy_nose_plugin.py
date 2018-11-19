import hy
import nose

from nose.plugins.base import Plugin


class HyFinderPlugin(Plugin):

    name = "HyFinderPlugin"

    # def options(self, parser, env):
    #     super(HyFinderPlugin, self).options(parser, env)

    def configure(self, options, config):
        super(HyFinderPlugin, self).configure(self, options, config)
        if not self.enabled:
            return

    def wantFile(self, file):
        return file.endswith('.hy')

    # def wantDirectory(self, directory):
    #     return True
    #
    # def wantModule(self, file):
    #     return True
