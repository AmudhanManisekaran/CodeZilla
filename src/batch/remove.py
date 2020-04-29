# @authors {Mayank Rawat}, @version 1.0
# @purpose {Deletes token.tok and token.pt files}
# @date 04/28/20

import os

os.chdir('../../data')
os.remove("token.pt")
os.remove("token.tok")
