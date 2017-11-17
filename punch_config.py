__config_version__ = 1

GLOBALS = {
    'serializer': '{{version}}',
}

FILES = ['version.txt']

VERSION = ['version']

VCS = {
    'name': 'git-flow',
    'commit_message': "Version updated from {{ current_version }} to {{ new_version }}",
}
