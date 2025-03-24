# Testing

**Checking functionality:**

``` bash
# Install coverage first
python -m venv venv
source venv/bin/activate
pip install -r requirements

# Running only the unittest
python -m unittest discover

# Checking test coveraging
coverage run -m unittest discover
coverage report -m
coverage html
```