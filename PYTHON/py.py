

x = [1, 2, 3, 4, 5]

import numpy as np

np_x = np.array(x)
np_y = 2 * np_x

print(np_y) 

import matplotlib.pyplot as plt

plt.plot(np_x, np_y)
plt.show()


@contextlib.contextmanager
def database(url):
  # set up database connection
  db = postgres.connect(url)
  
  yield db

  # tear down database connection
  db.disconnect()

url = 'http://datacamp.com/data'
with database(url) as my_db:
  course_list = my_db.execute(
    'SELECT * FROM courses'
    )
    
@contextlib.contextmanager
def timer():
  """Time the execution of a context block.

  Yields:
    None
  """
  start = time.time()
  # Send control back to the context block
  yield None
  
  end = time.time()
  print('Elapsed: {:.2f}s'.format(end - start))

with timer():
  print('This should take approximately 0.25 seconds')
  time.sleep(0.25)
