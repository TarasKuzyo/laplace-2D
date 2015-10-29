import sys
import numpy as np
import matplotlib.pylab as pl
import matplotlib




def read_config(filename):
    with open(filename) as f:
        lines = f.readlines()
        
    for line in lines:
        
        if   'x-dir' in line:
            x0, x1, nx = map(float, filter(lambda x: x, line.split('x-dir', 1)[1].split(' ')) )
        elif 'y-dir' in line: 
            y0, y1, ny = map(float, filter(lambda x: x, line.split('y-dir', 1)[1].split(' ')) )
        
        
    return ((x0, y0), (x1, y1), (nx, ny))
    

if len(sys.argv) < 2:
    sys.exit('usage: {} source-file'.format(sys.argv[0]))

source = sys.argv[1]
data   = np.fromfile(source, dtype='d')

config_file = 'laplace.ini'
(x0, y0), (x1, y1), dim = read_config(config_file)

data = data.reshape(list(reversed(dim))).transpose()



      
pl.imshow(data, origin='lower', extent=[x0, x1, y0, y1], interpolation='none')
pl.colorbar(shrink=1.0)

pl.savefig(source.rsplit('.', 1)[0] + '.png', dpi=300, bbox_inches='tight')

