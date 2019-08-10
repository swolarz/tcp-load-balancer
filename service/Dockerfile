FROM python:3.7-alpine

COPY requirements.txt ./

RUN pip install -r requirements.txt

WORKDIR /usr/app
COPY service.py /usr/app/

EXPOSE 5000
ENTRYPOINT ["python", "service.py"]
