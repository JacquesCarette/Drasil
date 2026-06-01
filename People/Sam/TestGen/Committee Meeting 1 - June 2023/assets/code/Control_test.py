## \filename ControlTest.py
#  \author Samuel J. Crawford
#  \brief Runs tests for the program
from os import path, remove
import sys
from pytest import mark
from pathlib import Path
from errno import ENOENT

from python import Control
from .TestHelpers import get_expected, read_file
from .test_input.expected_outputs import invalid_value_input_files

output_filename = "output.txt"

# from https://stackoverflow.com/questions/54071312/how-to-pass-command-line-argument-from-pytest-to-code
## \brief Tests main with valid input file
@mark.parametrize("filename", get_expected())
def test_main_valid(monkeypatch, filename):
    with monkeypatch.context() as m:
        m.setattr(sys, 'argv', ['Control.py', str(Path("test/test_input") / f"{filename}.txt")])
        Control.main()
    assert read_file(output_filename) == read_file(str(Path("test/test_output") / f"{filename}.txt"))

# from https://stackoverflow.com/questions/54071312/how-to-pass-command-line-argument-from-pytest-to-code
## \brief Tests main with invalid input file
@mark.parametrize("filename", invalid_value_input_files)
@mark.xfail
def test_main_invalid(monkeypatch, filename):
    # from https://stackoverflow.com/questions/10840533/most-pythonic-way-to-delete-a-file-which-may-not-exist
    try:
        remove(output_filename)
    except OSError as e: # this would be "except OSError, e:" before Python 2.6
        if e.errno != ENOENT: # no such file or directory
            raise # re-raise exception if a different error occurred

    assert not path.exists(output_filename)

    with monkeypatch.context() as m:
        m.setattr(sys, 'argv', ['Control.py', str(Path("test/test_input") / f"{filename}.txt")])
        Control.main()
    
    assert not path.exists(output_filename)
