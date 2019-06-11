import flask
from flask import Flask
import json
import random
import time
import threading


app = Flask(__name__)

sess_cnt_lock = threading.Lock()
sessions = 0


@app.route('/')
def serve():
    global sessions
    global sess_cnt_lock

    # print('### Serving request from: {}:{}'.format(
    #     flask.request.remote_addr,
    #     flask.request.environ.get('REMOTE_PORT')
    # ))

    serve_millis = app.config['serve_millis']
    min_serve_millis, per_req_serve_millis = serve_millis

    with sess_cnt_lock:
        sessions += 1
        sess_num = sessions

    serve_base_time = min_serve_millis + sess_num * per_req_serve_millis
    serve_time = random.randint(serve_base_time, serve_base_time + 50)

    time.sleep(serve_time / 1000)

    # print('### Served in: {} millis'.format(serve_time))

    resp = { 'status': 'OK' }
    resp = json.dumps(resp)

    with sess_cnt_lock:
        sessions -= 1

    return resp


def main():
    min_serve_millis = random.randint(200, 500)
    per_req_serve_millis = random.randint(50, 100)
    serve_millis = (min_serve_millis, per_req_serve_millis)

    print('Started service with delay params: {}'.format(serve_millis))

    app.config['serve_millis'] = serve_millis
    app.run(host='0.0.0.0')


if __name__ == '__main__':
    main()
