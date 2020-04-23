#!/usr/bin/python
# coding: utf-8
import time
from functools import partial, wraps


def console_log(*args, **kwargs):
    print(*args, **kwargs, flush=True)


class PrintDuration:
    """
    Prints time elapsed during an operation. Prints title of the operation, when its execution starts and ends. When it ends, it also prints the total time elapsed between start and end.
    This class is meant to be used as a With Statement Context Manager. Here is an example:
    ```
    with PrintDuration("My task"):
        calling_a_function(parameters)
    ```
    """
    def __init__(self, title=None, print_function=None):
        self.title = title or "Operation"
        if print_function is None:
            self.print_function = print
        else:
            self.print_function = print_function

    def __enter__(self):
        self.timing1 = time.perf_counter()
        self.print_function(self.title + ": Starting execution")

    def __exit__(self, exc_type, exc_val, exc_tb):
        timing2 = time.perf_counter()
        self.print_function(self.title + ": Execution complete. Duration: " + str(timing2 - self.timing1) + " seconds")


"""
A particular case of PrintDuration, which uses `console_log()` as its `print_function`.
This class is meant to be used as a With Statement Context Manager. Here is an example:
```
with ConsoleLogDuration("My task"):
    calling_a_function(parameters)
```
"""
ConsoleLogDuration = partial(PrintDuration, print_function=console_log)


def try_several_times(max_attempts, sleep_duration=1):
    """
    `sleep_duration` is in seconds
    """
    def decorator_try_several_times(func):
        @wraps(func)
        def wrapper_try_several_times(*args, **kwargs):
            current_attempt = 1
            while(current_attempt <= max_attempts):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    console_log(f"Attempt {current_attempt} failed. Error was:", e)
                    current_attempt += 1
                    time.sleep(sleep_duration)
            if current_attempt > max_attempts:
                raise Exception(f"Error. Failed after {current_attempt-1} attempts.")
        return wrapper_try_several_times
    return decorator_try_several_times
