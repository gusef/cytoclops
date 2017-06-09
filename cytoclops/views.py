from cytoclops import app

from fcsio.cli.utilities.simulate import simulate
from fcsio import FCS

from plotly.offline import iplot, plot
import plotly.graph_objs as go

@app.route('/')
def index():
   v = FCS(simulate(number_of_events=100000))
   mat = v.data.matrix
   xvals = [x[1] for x in mat]
   yvals = [x[2] for x in mat]
   data = go.Histogram2dContour(
          x=xvals,
          y=yvals,
          )
   data2 = go.Scatter(x=xvals,
                      y=yvals,
                      mode="markers",
                      opacity=0.2,
                     )
   #fig = dict(data=data)
   fig=dict(data=[data],
            layout=go.Layout(height=600,
                             width=600,))
   return plot(fig,
               output_type='div',
              )
