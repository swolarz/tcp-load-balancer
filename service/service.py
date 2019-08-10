import flask
from flask import Flask
import websocket
import json
import random
import time
import threading
import argparse
import urllib.parse as urlparse


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

    monitoring_ws = app.config['monitoring_ws']

    serve_millis = app.config['serve_millis']
    min_serve_millis, per_req_serve_millis = serve_millis

    with sess_cnt_lock:
        sessions += 1
        sess_num = sessions
    
    monitoring_ws.send(json.dumps({
        'reqs': sess_num
    }))

    serve_base_time = min_serve_millis + sess_num * per_req_serve_millis
    serve_time = random.randint(serve_base_time, serve_base_time + 50)

    time.sleep(serve_time / 1000)

    # print('### Served in: {} millis'.format(serve_time))

    resp = { 'status': 'OK' }
    resp = json.dumps(resp)

    with sess_cnt_lock:
        sessions -= 1

    return resp


def prepare_monitoring_websock_url(base_url, instance_name):
    print('Base url: {}\nInstance: {}'.format(base_url, instance_name))

    url = urlparse.urlparse(base_url)
    query = urlparse.parse_qs(url.query)
    query.update({'instance': instance_name})
    querys = urlparse.urlencode(query)

    url = url._replace(query=querys)
    url = url.geturl()

    # url = 'ws://127.0.0.1:8080/listener?instance=test-instance'

    print('Websocket url: {}'.format(url))

    return url


def establish_monitoring_connection(ws_url):
    print('Establishing connection to monitoring service...')

    delay_ms = 1000
    trials = 0
    connected = False

    while not connected:
        try:
            time.sleep(delay_ms / 1000)
            delay_ms = min([5000, delay_ms * 2])

            ws = websocket.create_connection(ws_url)
            connected = True

        except OSError:
            if trials == 3:
                raise

            trials += 1
            print('Retrying to establish connection...')

    return ws


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--host', default='0.0.0.0', required=False)
    parser.add_argument('--port', default='5000', type=int, required=False)
    parser.add_argument('--name', required=True)
    parser.add_argument('--monitoring', required=True)
    parser.add_argument('--discovery', required=False)
    
    args = parser.parse_args()

    # time.sleep(2)

    min_serve_millis = random.randint(200, 1000)
    per_req_serve_millis = random.randint(50, 100)
    serve_millis = (min_serve_millis, per_req_serve_millis)

    instance_name = '{}_{}'.format(args.name, serve_millis)
    monitoring_ws_url = prepare_monitoring_websock_url(args.monitoring, instance_name)

    monitoring_ws = establish_monitoring_connection(monitoring_ws_url)

    print('Started service with delay params: {}'.format(serve_millis))

    app.config['serve_millis'] = serve_millis
    app.config['monitoring_ws'] = monitoring_ws

    app.run(host=args.host, port=args.port)


if __name__ == '__main__':
    main()

