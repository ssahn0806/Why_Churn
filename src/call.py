import requests
import json
import pandas as pd

def get_data1():
    dataset = {'count':0,'entry':[],'totalResult':0}

    key = 'd27cced29d2b5cf46f832215277f0df5303b2d03ce93277ff10b6f9c87c244b0'
    page = 1
    count = 1000

    url = f'https://api.bigdatahub.co.kr/v1/datahub/datasets/search.json?pid=1002346&TDCAccessKey={key}&$page={page}&$count={count}'
    response = requests.get(url)
    if response.status_code == requests.codes.ok :
        data = json.loads(response.text)
        dataset['totalResult'] = data['totalResult']
    else :
        print("Error")
        return 0

    while dataset['count'] < dataset['totalResult'] :
        print(url)
        url = f'https://api.bigdatahub.co.kr/v1/datahub/datasets/search.json?pid=1002346&TDCAccessKey={key}&$page={page}&$count={count}'
        response = requests.get(url)
        if response.status_code == requests.codes.ok :
            data = json.loads(response.text)
            dataset['count'] += data['count']
            dataset['entry'].extend(data['entry'])
            page +=1
        else :
            break
    print(len(dataset['entry']))
    df = pd.DataFrame(dataset['entry'])
    df.to_csv('./202012.csv',encoding='utf-8-sig')
def get_data2():
    dataset = {'count':0,'entry':[],'totalResult':0}

    key = 'd27cced29d2b5cf46f832215277f0df5303b2d03ce93277ff10b6f9c87c244b0'
    page = 1
    count = 1000

    url = f'https://api.bigdatahub.co.kr/v1/datahub/datasets/search.json?pid=1002283&TDCAccessKey={key}&$page={page}&$count={count}'
    response = requests.get(url)
    if response.status_code == requests.codes.ok :
        data = json.loads(response.text)
        dataset['totalResult'] = data['totalResult']
    else :
        print("Error")
        return 0

    while dataset['count'] < dataset['totalResult'] :
        print(url)
        url = f'https://api.bigdatahub.co.kr/v1/datahub/datasets/search.json?pid=1002283&TDCAccessKey={key}&$page={page}&$count={count}'
        response = requests.get(url)
        if response.status_code == requests.codes.ok :
            data = json.loads(response.text)
            dataset['count'] += data['count']
            dataset['entry'].extend(data['entry'])
            page +=1
        else :
            break
    print(len(dataset['entry']))
    df = pd.DataFrame(dataset['entry'])
    df.to_csv('./201912.csv',encoding='utf-8-sig')        
def anaysis():
    df = pd.read_csv('./201912.csv')
    print(df.info())
    columns = df.columns
    print(df[f'{columns[2]}'].unique())
if __name__ == "__main__" :
    #get_data1()
    #get_data2()
    anaysis()