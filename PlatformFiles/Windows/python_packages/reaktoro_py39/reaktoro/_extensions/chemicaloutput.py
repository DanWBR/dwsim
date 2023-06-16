import numpy as np
import pandas as pd
from reaktoro.PyReaktoro import ChemicalOutput


def _ChemicalOutput_to_array(self):
    """
    Define a method to convert the file data into an array.

    :return:
        An numpy array with data from ChemicalOutput.
    :rtype numpy.ndarray:
    """
    output_array = np.loadtxt(self.filename(), skiprows=1)
    return output_array


def _ChemicalOutput_to_dict(self):
    """
    Define a method to convert the ChemicalOutput data in the file into a dictionary,
    with the headings being the keys, and the column data as the values.

    :return:
        A dict with ChemicalOutput properties from result file.
    :rtype dict:
    """
    data = self.to_array()
    columns = [data[:, i] for i in range(data.shape[1])]
    return {heading: column for heading, column in zip(self.headings(), columns)}


def _ChemicalOutput_to_dataframe(self):
    """
    Convert ChemicalOutput data into a pandas DataFrame.

    :return:
        A pandas DataFrame with ChemicalOutput data.
    :rtype pd.DataFrame:
    """
    output_dataframe = pd.DataFrame(self.to_dict())
    return output_dataframe


ChemicalOutput.to_array = _ChemicalOutput_to_array
ChemicalOutput.to_dict = _ChemicalOutput_to_dict
ChemicalOutput.to_data_frame = _ChemicalOutput_to_dataframe
