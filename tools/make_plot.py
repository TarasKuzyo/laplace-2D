import sys
import numpy as np
import matplotlib.pylab as pl
import matplotlib




def read_config(filename):
    with open(filename) as f:
        lines = f.readlines()
        
    for line in lines:
        if 'dimensions' in line:
            ndim = int(line.split('dimensions', 1)[1])
        elif 'x-dir' in line:
            x0, x1, nx = map(float, filter(lambda x: x, line.split('x-dir', 1)[1].split(' ')) )
        elif 'y-dir' in line: 
            y0, y1, ny = map(float, filter(lambda x: x, line.split('y-dir', 1)[1].split(' ')) )
        elif 'z-dir' in line:
            z0, z1, nz = map(float, filter(lambda x: x, line.split('z-dir', 1)[1].split(' ')) )
        
    return (ndim, [x0, y0, z0][:ndim], [x1, y1, z1][:ndim], [nx, ny, nz][:ndim])
    

if len(sys.argv) < 2:
    sys.exit('usage: {} source-file'.format(sys.argv[0]))

source = sys.argv[1]
data   = np.fromfile(source, dtype='d')

config_file = 'laplace.ini'
ndim, start, end, dim = read_config(config_file)

data = data.reshape(dim).transpose()

if ndim == 1:
    pl.plot(data)
    pl.show()

if ndim == 2:
    pl.title('f(y), x = 0')
    pl.xlim([0, data.shape[0]])
    pl.plot(data[0, :])
    pl.show()
    pl.title('f(y), x = 1')
    pl.plot(data[-1, :])
    pl.xlim([0, data.shape[0]])
    pl.show()
    pl.title('f(x), y = 0')
    pl.xlim([0, data.shape[1]])
    pl.plot(data[:, 0])
    pl.show()
    pl.title('f(x), y = 1')
    pl.xlim([0, data.shape[1]])
    pl.plot(data[:, -1])
    pl.show()
    
if ndim == 3: 
    pl.title('f(x, y), z = 0')
    pl.imshow(data[:, :, 0], origin='lower')
    pl.colorbar(shrink=1.0)
    pl.show()
    pl.title('f(x, y), z = 1')
    pl.imshow(data[:, :, -1], origin='lower')
    pl.colorbar(shrink=1.0)
    pl.show()
    pl.title('f(y, z), x = 0')
    pl.imshow(data[0, :, :], origin='lower')
    pl.colorbar(shrink=1.0)
    pl.show()
    pl.title('f(y, z), x = 1')
    pl.imshow(data[-1, :, :], origin='lower')
    pl.colorbar(shrink=1.0)
    pl.show()
    pl.title('f(x, z), y = 0')
    pl.imshow(data[:, 0, :], origin='lower')
    pl.colorbar(shrink=1.0)
    pl.show()
    pl.title('f(x, z), y = 1')
    pl.imshow(data[:, -1, :], origin='lower')
    pl.colorbar(shrink=1.0)
    pl.show()

      


#pl.imshow(data, origin='lower', extent=[0.0, 1.0, 0.0, 1.0], interpolation='none')
#pl.colorbar(shrink=1.0)

#pl.savefig(source.rsplit('.', 1)[0] + '.eps', dpi=300, bbox_inches='tight')

