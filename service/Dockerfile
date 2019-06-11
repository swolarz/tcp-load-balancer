FROM python:3.7-alpine

RUN pip install flask

WORKDIR /usr/app
COPY service.py /usr/app/

EXPOSE 5000
ENTRYPOINT ["python", "service.py"]
