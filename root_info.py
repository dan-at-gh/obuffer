#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
from subprocess import call, run, PIPE
import re
import psutil
import time
import sys

home = os.path.expanduser('~')
database = os.path.join(home, '.obuffer.db')
symlink_database = os.path.join(home, '.obuffer-symlink.db')
FNULL = open(os.devnull, 'w')
path_keys = ['.hg', '.git', '.obuffer']
backend_calls = {'hg': {'revision': ['hg', 'id', '-n'],
                        'status': ['hg', 'status']},
                 'git': {'revision': ['git', 'rev-list', '--count', 'HEAD'],
                         'status': ['git', 'status', '-s']}}
exclude_paths = ['~/.local/share/Trash']


def check_output(cmd_lst, stdout=PIPE, stderr=FNULL):
    return run(cmd_lst,
               stdout=stdout,
               stderr=stderr).stdout.decode("utf-8")


#** Version control

def project_file_states(backend):
    if backend in backend_calls:
        states = []
        try:
            lines = check_output(backend_calls[backend]['status'],
                                 stderr=FNULL).splitlines()
            for line in lines:
                states.append(line.split()[0])
            if states:
                states = list(set(states))
                return ''.join(states)
            return 'OK'
        except:
            return '--'
    return '--'


def allowed_path(path):
    for exclude in exclude_paths:
        if os.path.expanduser(exclude) in path:
            return False
    return True


def locate_regexp():
    re_parts = []
    for key in path_keys:
        re_parts.append('/\\{}$'.format(key))
    return '\\|'.join(re_parts)


def revision_number(backend):
    if backend in backend_calls:
        try:
            out = re.sub('[^0-9]$', '',
                          check_output(backend_calls[backend]['revision'],
                                       stderr=FNULL)).strip()
            if out:
                return out
            else:
                return '--'
        except:
            return '--'
    else:
        return '--'


def nested_repo_levels(sorted_paths):
    parents = []
    repos_levels = []
    for path in sorted_paths:
        level = 1
        for parent in parents:
            if os.path.join(parent,'') in path:
                level += 1
        repos_levels.append([path, level])
        if level == 1:
            parents[:] = []
        parents.append(path)
    return repos_levels


def project_roots():
    wd = os.getcwd()
    call(['updatedb', '-l', '0', '-o', database, '-U', home])
    paths = []
    backends = {}
    infos = []
    for root in check_output(['locate',
                          '-d', database,
                          '-r', locate_regexp()]).splitlines():
        path, backend = os.path.split(root)
        if allowed_path(path):
            paths.append(path)
            backend = backend.lstrip('.')
            if backend in backend_calls:
                backends[path] = backend
            else:
                backends[path] = '--'
    paths.sort()
    for path, level in nested_repo_levels(paths):
        os.chdir(path)
        backend = backends[path]
        rev = revision_number(backend)
        state_summary = project_file_states(backend)
        if os.path.isfile(os.path.join(path, '.closed')):
            project_state = 'closed'
            level += 1
        else:
            project_state = 'running'
        path = os.path.join(path, '')
        infos.append(' '.join([path, backend, rev, str(level),
                               state_summary, project_state]))
    os.chdir(wd)
    return infos

        
#** Main

if len(sys.argv) == 2:
    sleep_interval = float(sys.argv[1])
else:
    sleep_interval = 0

daemon = True
while daemon:
    with open(os.path.join(home, '.obuffer.log'), 'w') as f:
        for line in project_roots():
            f.write('{}\n'.format(line))

    if sleep_interval > 0:
        time.sleep(sleep_interval)
    else:
        daemon = False

