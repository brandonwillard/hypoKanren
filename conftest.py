import pytest
import hy


def pytest_collect_file(parent, path):
    if path.ext == ".hy" and path.basename != "__init__.hy":
        pytest_mod = pytest.Module(path, parent)
        return pytest_mod
