# %%
from __future__ import annotations
from typing import Literal, Optional
import json
import base64
import zlib
import sys
from numpy import ndarray

import numpy as np

SYS_ENDIAN = sys.byteorder == 'little'

COLORS = Literal[
    'Luma8',
    'LumaA8',
    'Rgb8',
    'Rgba8',
    'Luma16',
    'LumaA16',
    'Rgb16',
    'Rgba16',
    'Rgb32F',
    'Rgba32F',
]

class DeserialImage:
    """# DeserialImage
    This class is used to decode an image from a JSON string generated
    from a `DynamicImage` object in the [`serialImage`](https://crates.io/crates/serialimage) 
    crate, and store it in a `numpy` array, along with any metadata that was included.

    ## Static Methods
    - `from_json(json_str: str) -> DeserialImage`: This method takes a JSON string and returns a `DeserialImage` object.
    """
    def __init__(self, data: ndarray, metadata: dict = None):
        """## Create a new `DeserialImage` object.

        ### Args:
            - `data (ndarray)`: The image data.
            - `metadata (dict, optional)`: Included metadata. Defaults to None.
        """
        self._data = data
        self._metadata = metadata

    @property
    def data(self) -> ndarray:
        """## Access the image data.

        ### Returns:
            - `ndarray`: The image data.
        """
        return self._data

    @property
    def metadata(self) -> Optional[dict]:
        """## Access the metadata.

        ### Returns:
            - `Optional[dict]`: The metadata, if any was included.
        """
        return self._metadata

    @staticmethod
    def from_json(json_str: str) -> DeserialImage:
        """## Create a `DeserialImage` object from a JSON string.

        ### Args:
            - `json_str (str)`: The JSON string to decode.

        ### Raises:
            - `ValueError`: If the JSON string is invalid.

        ### Returns:
            - `DeserialImage`: The decoded `DeserialImage` object.
        """
        data = json.loads(json_str)
        if 'data' not in data or 'width' not in data or 'height' not in data or 'color' not in data or 'le' not in data or 'metadata' not in data:
            raise ValueError('Invalid JSON string')
        imgdata = base64.b64decode(data['data'] + '==', validate=True)
        imgdata = zlib.decompress(imgdata)
        col = data['color']
        if col == 'Luma8' or col == 'LumaA8' or col == 'Rgb8' or col == 'Rgba8':
            dtype = np.uint8
        elif col == 'Luma16' or col == 'LumaA16' or col == 'Rgb16' or col == 'Rgba16':
            dtype = np.uint16
        elif col == 'Rgb32F' or col == 'Rgba32F':
            dtype = np.float32
        else:
            raise ValueError(f'Invalid color format {col}')
        imgdata: np.ndarray = np.frombuffer(imgdata, dtype=dtype)
        if data['le'] != SYS_ENDIAN:
            imgdata = imgdata.byteswap(inplace=True)
        imgdata = imgdata.reshape(data['width'], data['height'], -1)

        return DeserialImage(
            imgdata, data['metadata']
        )


# %%
if __name__ == '__main__':
    import matplotlib.pyplot as plt
    img = DeserialImage.from_json('{"data":"eJxFmVlMk2kbhuewuIK48KMS64oRFVxGUDiwKioEt2hUVCKMGhhccS09tEWNoEYBNyBuaDRWRalBhQPBVESCLAkgCJEAA4RlCOshPxc3f/5JZq4Raftdb7/vfZ/nfoaGhob+GP4nd1CM/1f0bxEH6sXsSjGuRPR1il154kuH+Ldd9M4U/0kTnySLUYniLKtYbxHTTovhsaLHX2JluJi8Q9wRIrqaxJIAMdFPDFkoGoyi00O0uoomg/jHH+LQUO6gyJ9trosGxd1d4sVm8fUvsbZCNHwXV+SLBz+IiW/EnGdiU4bolioGJYkxVjHFIn6OEztjRM9IMXiPGLdVzAgWi4LEgRXiHB9x6xzR4ik+cxMrDKK8Ye6gycCf4v+V/6JBfq51cDFeHBANnWJSozipRkwtFad/FR/kifOyxecvxCUPxXe3Rf9rYp5VXGsRnafE0GixNELcuUv8GSpGmMRGfzF6qdgxTzw1QxyYJFpcRHnDRYMVBvzj/7WOrIZ/i/x3d8n/4gC/p3VI8vvcL1rbxeAG0aVKLC4Wr+WL23NE91di5WPxzl1x33XRyyb+jhcfnxQPHxG994vt20X7RvFkkLhsudjvLeZ4ifGTxaAxorzhxQGLC/67u5654e/f4vTgbwfq5X+xWf6GTvl/7ud1Wgc3U12fmNsm3qsXzRXinkLxzzxxylux76lYfl/MuiHesInH48WwE6LPIXHMXrEtTCw0iU9XiTYf8ZBRNE0VjWNFecPP/UFj8Dd0DkzC/2KzxRP/gXrDyF2RXSn/17/kn9Qof2u7/Ov6eB+tQ8qOkl4xvUU8WisG/hDHFog170U9Byk7zBnippvitASxxSy+PybaIkXd9yk75m4WewPFfF/x5lwx0kP0HSfKG9b1Gcfib22Pn4x/UuOpGfi//rV1Dv7ZlSEL+e24EvnXVsh/Uo38gxvkn9sm/5Je3lfr8J+/bD3iuGbxVpXoWSQ+yBUXvBZfPRRXJou5CaLJLH6LFbdFiFXbxIh1YtNKMdZb7PEUzeNFecOSXt9x+Oe2mabiH9yQ44X/pJqOefjXVszxwT+uJHHk6fB1yt/wXf6ppfJ3qZL/vXr5p7fI39bD52gd0k93d4sNDWJ5uVhQIL7LFp9kiimpYsIl8fwFMTpG3BsuhoSKawJFn8XiTC9xwkRR3tDWYx6Pf3pLpAf+9+oPGfF3qer3xj+1NHop/obvAyvw93WWBPDqrjz5r8iX//Sv8i8ulr+5Qv5Ha+U/rln+3d18rtbBaO3qEuvqRO2LRuvHT6KeA6NV+6DReumyeO68qH3PaNV9b7SuWy9qnzNajbNFVzdR3rC7e8JE/Mc193jif7T25lz8zRU2H/yLi5ctx3/610Z//FfkFwXh35XnOrJLvHTI/+AH+T/Ik/+1fPnvKZR/4A/536qSf0OD/Lu6uA6tQ2ZyWIeYVi22fxFXZ4lX0sXqK+L8c+LZKPHLFtF9jRi1QMxyF+UNu7pc3fBvaJjphf+tqlhv/AN/5Pviv6fw6Sr8r+WfDML/QV6ECf+DHzKC8X/p2BHCu/1tl3/iG/nPy5b/9hz5/5kn/7EF8vcskn95ufzr6uQf1sF1aR0WZn5oFU+XiYs/iq2PxEdXxf1nxKkHxLIN4tUl4oZporxhWEeWO/51dcbZ+JeX+yzG37OoaSX+Ywt6A/H/M6/QhP/2HPtG/Odl/wzFP/FN3Fb8/7Ynj+yW3pnyz3km/+cv5O/+Sv5T3sq/5r38H+TKv6BA/jwn+KdVy/9DK9epdbA7VDfZHaqT7A49B3aH6iC7Q3WP3aH73u5QXWN3qI6xO+QNP7RumIZ/WnXUAvy5r/EvKFgTiP+D3Ih1+Ne8n7sZ/ylv28Lwd3/Vvh3/5y927sI/51nwHvy9MyvDefd/0uTflCH/JQ/lX/lY/n1P5c+64L/gtfzfZcv/4yf5t3+R/+ky+XOO2h1aBz+nzkk/p85FP6fOQT+nzj0/p845P6fONT+nvCHnFv6ny64uwb/9i/sa/D9+Wrce/3fZIaH4L3hdtQ1/PPHvezpmL/6Vj73347/kYWkE/k0ZnpH4/5PmMXJqPEmWv1uq/N/dlv+du/Ivvy9/c4b8Xz2U/5NM+bMu+K/Okv/ij/JnH8WffdPPqXVwVGpfdFRqH3RUat9zVGqfc1TKG7KP4c++hf/ij2Ub8F+d9WUL/nji/yRzbzj+rx5ui8DfnGGLxL/8vs8h/O/cPXwE/3e3Q6Pxd0vtjMH/SXJ4LJ8WlSj/oCT5+1+T/77r8s+6If9NN+W/Mln+KanyZ53wv5Iu/9ZH8mdd8Oe5wZ/nxFGpdQho0XMQ0KL7PqBF3pD7Gn/uY/zxxL/10dQD+F9JPxuFP174p6RGx+C/MvlbLP6bbr4/hn/WjbAT+O+7/vgk/v7XnKfwD0r6HId/VGLayOk5yyr/GKv886zy97LJ/4ZN/tMS5J+bIP+ES/K/dFn+1Vfk/+iq/FlH/Fk3/Fkn/FmXgBatQ96gvCGe+OOFPx74c934P7q6/wz+1Vfmn8P/0uVz5/FPuHT+Av65CSYz/tMSWsz437Adj8ffy/Y7Hv8861oL/jHWFAv+s6z1Fj6d/8IUi/zXWuT/O17+x+Pl32KWv8ks//MX5H/uvPznn5P//jPyd56S//tj8j98RP47d8mfT80b7b3hzl3PX+B/+Midu/i/P7bpJv7OU/7X8N9/5tFV/Oefq76C/7nzly7jf/5CwiX8TebcBPxbzNMS8D8ef8OG/+94Lxv+ay15VvxTLDFW/Osts0aqCO4C+DlO/lw3/o9Pyj/shPzxwP9brPyjY+SPF/5no+Q/9YD88cR/7mb5L1sufz6N+x9j7n+++YDRHhQuW15cjP/czTXv8Wdd8J96oPUR/mejrqTjzzrhHx2Tkor/t9iVyfizbviHnci6gf/jk/uu48864v85LigJ/7TTPPlDQ+wCsDNG/qHR8scLf59D8rdFyn9bhPz3hssfA/y/bJF/2Qb5N/rL/+Zc+fMp7H8Ysv9hwP7HHc/+x5PvGO3F4M25R2vxb/Sf/hX/sg2LP+L/ZcvqLPxZF/z3hj/JxH9bxKuH+NsizRn4+xwqv48/64R/aPS72/h3xril4h8e+2SkmuIUgJ6R8i+NkL/3fvmP2St/PPGv2ib/kFD5r1svf/c18r+6RP6nZsifd+f8w4jzj2+W849vivOPJ53zjx2P84+d32+0J4GnZiQ14n91yeky/N3XtH/Bf936j5/wDwl9l41/1bYFr/FnXfAfs7fvKf7e+ysf418aseQh/p6RTRn4e/zFyT98zofrs4L3yB9P/Nu3y78tTP5cIf4R6+S/JlD+fHP4Ry2Q/4Zp8uddqX8woP7hm6T+4Qqpf7gjqX/Y4ah/eA6ofzjxqH84+e2jtTncMO1DK/5RC9Kq8ec5wX9NYEEB/hHrHuTiz3ODf1vYlLf4t293f4U/64J/8J6cZ/hXhnuPVJVUgSN98Fb5/wyVv32j/AtN8u8NlH/TSvn7LJa/cbb8s9zlz7tR/3LF1L98c9S/3MHUvzzJ1L/saNS/7OzUv5xw1L+c9NS/VDzUv1R+C0drVJjlHtaBv3H2cAU87O+zuLwc/6aVnkX49waOLcC/cDiLwd++cXsO/j9D52XjH7c18Q3+yTuo/IeG6AJgRrD8I0zyPxkk/6er5J/vK/9Yb/nP9JK/q5v8eRf6H66Q/odviv6HO5b+hyeX/ocdjP6HnZz+hxON/oeTnf6HCof+h0qP/oeKl/6Hyj9ztFaDrm5dXfjP9GpowD/W+1YV/vm+gT/wf7pqTyH+J4Ou5eMfYXqQh39G8MEP+O8IeTlSXdMFwqIg+XMH4s99jb/NR/48mfj3eMp/wkT582r6X66I/pdvhv6XO5T+lyeV/pc7kP6XHYn+lxOM/peTnP6XfZD+l+eA/pcKl/6XfZH+d6TjsbLa8uaa4YSJ3d3493iOa8affRN/m4+5An+eE/zZR/EvClqRj7+ric5/ONcJ0HsNrJB/9FL593vL/5BR/pEe8jePlz+vIv/gCsg/+CbIP7gjyT94Msk/2KHIP9ipyT84scg/OLnJP6hgyD+o5Mg/qGjJP6jsyT/ocMg/6PTIP+h4yT/o/NNHz25oHm/rwT/SI70F/0PGe/X493u7VOEfvTS1FP+BFYbv+JcE+I50GaRAcI6P/DvmyT/HS/6mqfL3HSd/fpv8i08k/2Llyb+4A8m/eBLJv9iRyL/Ymcm/OKHIvzipyb+oWMi/qNzIv6hgyb+o5Mm/6GjIv+jsyL/ocMm/6PTJv0g8yL9Ifv4zeoZB33Elvfibpua24Z/jFdyAf8e8STX4z/GprcA/0Y/kb/gcW6jXbp0jf3Ze/OMny984Vv78Fvknn0D+yUqTf3LHkX/y5JF/sgORf7ITk39y35N/cjKTf3LukX9SqZF/UrGSf3IOkn/SwZB/8hyQf3Iukn/S2ZN/knCQf3JOkn+SeJF/kvyljO7l0Di2rg//+MnWdvw5R/HfOuf1L/xDFmaPdFukwNDiKf+BSfIPGiN//pb8m3ck/2Zlyb+5w8i/edLIv9lxyL/Zecm/OYHIvzmJyb+pSMi/qczIv6lQyb+p1Mm/6VjIv+ncyL/pYMm/6eTJv0k0yL9Jdsi/SbjIv0n6yL9JPMm/SX7dRvc0GDTmcz/+A5MMnfhbPC82428wkvwP17se+t1nbvK3uMifnzL/4B2Yf7CSzD+4o5h/8GQx/2CHYf7BTsv8gxOH+QcnL/MPKhDmH+x7zD+oSJl/UJkz/6BDYf5Bp8b8g32Q+QedO/MPEgzmHyQ5zD9ItJh/sC8y/yDhZP5B0sv8g8Sb+QfJf9Losw0tLhcH8H/mtrsLf6eH/0jXyRQIVhjkz5+Yf/EK5l+sHPMv7iDmXzxJzL/YUZh/sbMy/+KEYf7FScv8i/ue+ReVF/MvKlDmX9Q9zL/oSJh/0Zkx/6IOYv5Fp878i8SC+RfPAfMvEizmXyR5zL+ok5h/kewy/yLhZv5F3cT8i4kH8y8mPy6j9zisMCwaxN/qyuRveH8z/H8ayvyT32D+yUox/+SOYf7Jk8P8kx2E+Sc7KfNPThTmn5yszD+pMJh/Umkx/6TiZP5J5c38kw6E+SedGPNPOlLmn3TmzD9JKJh/ktQw/ySxYv5Jcsf8kwST+SdJLvNPEm3mnyT7zD+ZcDD/ZNLD/JOJF/NPJn+20e8amgy5g//zZhrKT5gKsjJMx7hDmBLxpDAtYcdgasDOSXrOCUKKzElKmkpFQapIZUW6RoVJykSlTdpCx0HqQOdF900Hqn/5f37G3/E7/C6v4bW8B+/Fe/LefAafxWfy2VwD18I1cW1cI9fKNXPtOOBidZUbjpp4y/2/Tn5zVQ","width":32,"height":32,"color":"Rgba16","le":true,"metadata":null}')
    data = img.data
    data = data / np.iinfo(data.dtype).max
    plt.imshow(data)
    plt.show()
# %%
