import matplotlib.pyplot as plt
import numpy as np

# extract data from file .txt and store it in a list /home/arthur/Desktop/TIPE/sortie_time.txt
# the syntax in the file is x : y; and you need to do 2 tab one with x and one with y
def extract_data(file):
    x = []
    y = []
    with open(file, 'r') as f:
        for line in f:
            line = line.split(':')
            x.append(float(line[0]))
            y.append(float(line[1]))
    return x, y


# plot the data
tab = extract_data('/home/arthur/Desktop/TIPE/sortie_time.txt')
plt.xlabel('taille de l entrée')
plt.ylabel('temps d éxecution en secondes')
plt.plot(tab[0], tab[1], '-')
plt.show()