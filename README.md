### How to run the project
Change working directory to your users. Then:

1. Run the Data file first always.
2. Run the desired question.
3. Reset environment and run next question.
4. Repeat.

**Reason**: The data is specifically loaded to use time periods within each file. As such, the original dataframes will be sub sets when first running the code; running the code again will sub set the sub set and so forth.


**Note**: The code is in R but the conversion of par-yields is in Python.
