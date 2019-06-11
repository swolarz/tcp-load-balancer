import requests
import asyncio
import threading
import time
import random
import argparse


SERVICE_URL = 'http://localhost:5000'


req_stats_lock = threading.Lock()
req_stats = {
    'avg_resp_millis': 0
}


def get_request_delay_millis():
    global req_stats, req_stats_lock

    with req_stats_lock:
        delay = req_stats['avg_resp_millis']

    delay += random.randint(-30, 30)
    delay = max([delay, 0])

    return delay

    
def update_req_resp_time(resp_millis):
    global req_stats, req_stats_lock
    
    with req_stats_lock:
        req_stats['avg_resp_millis'] = req_stats['avg_resp_millis'] * 0.5 + resp_millis * 0.5


@asyncio.coroutine
def consume_service(loop, service_url):
    while True:
        req_start = time.time()
        
        yield from loop.run_in_executor(None, requests.get, service_url)
        
        req_millis = int((time.time() - req_start) * 1000)
        update_req_resp_time(req_millis)

        yield from asyncio.sleep(get_request_delay_millis() / 1000)


@asyncio.coroutine
def log_request_delay():
    global req_stats, req_stats_lock

    yield from asyncio.sleep(2)

    while True:
        with req_stats_lock:
            print('\rRequest average delay: {:>10.2f} ms'.format(req_stats['avg_resp_millis']), end='')
            
        yield from asyncio.sleep(0.5)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--service', required=False, default='http://localhost:5000/')
    
    args = parser.parse_args()
    loop = asyncio.get_event_loop()

    for _ in range(16):
        asyncio.async(consume_service(loop, args.service))

    asyncio.async(log_request_delay())

    loop.run_forever()


if __name__ == '__main__':
    main()

