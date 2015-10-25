# InsecureDrop - a simple web-based dead-drop intended to be run as
#                a Tor hidden service
# Copyright (C) 2015 William Waites

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from flask import Flask, request, redirect, make_response
from flaskext.uploads import UploadSet, ALL, configure_uploads
from time import time
from os import listdir, path
from mimetypes import guess_type

PATH = '/home/cryptoparty'

app = Flask(__name__)

messages = UploadSet('messages', ALL, default_dest=lambda _: PATH)
configure_uploads(app, (messages,))

@app.route('/', methods=['GET'])
def form():
    return """\
<html>
  <head>
    <title>InsecureDrop</title>
  </head>
  <body>
    <h1>InsecureDrop</h1>
    Upload a new file:<br />
    <form method="POST" enctype="multipart/form-data">
      <input type="file" name="message" />
      <input type="submit" value="upload" />
    </form>
    <hr />
    %s
  </body>
</html>
""" % listing()

@app.route('/', methods=['POST'])
def upload():
    if 'message' in request.files:
        data = request.files['message']
        data.filename = '%d-%s' % (time(), data.filename)
        filename = messages.save(data)
        return """\
<html>
  <head>
    <title>Thank you</title>
  </head>
  <body>
    <h1>Thank you...</h1>
    ... your message has been saved
    <hr />
    %s
  </body>
</html>
""" % listing()
    else:
        return form()

@app.route('/<name>', methods=['GET'])
def get(name):
    try:
        fp =open(path.join(PATH, path.basename(name)), 'r')
        data = fp.read()
        fp.close()
        resp = make_response(data, 200)
        mimetype, _ = guess_type(name)
        if mimetype is not None:
            resp.headers['Content-Type'] = mimetype
        return resp
    except Exception, e:
        print e
        return make_response(E404, 404)
    
def listing():
    def list_():
        yield "<table>"
        for f in listdir(PATH):
            if f.startswith('.'): continue
            yield '<tr><td><a href="/%s">%s</a></td></tr>' % (f, f)
        yield "</table>"
    return "\n".join(list_())

E404 = """\
<html>
  <head>
    <title>Sorry, Not Found</title>
  </head>
  <body>
    <h1>404, eh</h1>
  </body>
</html>
"""

if __name__ == '__main__':
    app.run(port=8080)
