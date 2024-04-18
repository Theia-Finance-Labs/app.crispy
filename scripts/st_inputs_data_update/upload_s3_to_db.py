import os
import boto3
import pandas as pd
from sqlalchemy import create_engine

# Environment Variables for S3
S3_URL = os.environ["S3_URL"]
S3_PREFIX = os.environ["S3_PREFIX"]
S3_ACCESS_KEY = os.environ["S3_ACCESS_KEY"]
S3_SECRET_KEY = os.environ["S3_SECRET_KEY"]
S3_BUCKET = os.environ["S3_BUCKET"]
S3_REGION = os.environ["S3_REGION"]

# Environment Variables for PostgreSQL
POSTGRES_USERNAME = os.environ["POSTGRES_USERNAME"]
POSTGRES_PASSWORD = os.environ["POSTGRES_PASSWORD"]
POSTGRES_HOST = os.environ["POSTGRES_HOST"]
POSTGRES_PORT = os.environ["POSTGRES_PORT"]
POSTGRES_DB = os.environ["POSTGRES_DB"]

# S3 Client with Custom Endpoint
s3_client = boto3.client('s3',
                         endpoint_url=f'https://{S3_URL}',
                         region_name=S3_REGION,
                         aws_access_key_id=S3_ACCESS_KEY,
                         aws_secret_access_key=S3_SECRET_KEY)

# Database Engine
db_connection_string = f'postgresql://{POSTGRES_USERNAME}:{POSTGRES_PASSWORD}@{POSTGRES_HOST}:{POSTGRES_PORT}/{POSTGRES_DB}?sslmode=require'
engine = create_engine(db_connection_string)

def upload_files_from_s3_to_db():
    # List objects within the specified S3 bucket and prefix
    response = s3_client.list_objects_v2(Bucket=S3_BUCKET, Prefix=S3_PREFIX)
    
    for obj in response.get('Contents', []):
        file_key = obj['Key']
        if file_key != 'st_inputs/abcd_stress_test_input.csv':
            file_name = file_key.split('/')[-1]  # Extract the filename
            
            if file_name.endswith('.csv'):  # Ensure it's a CSV file
                # Download the file from S3 to a pandas DataFrame
                obj = s3_client.get_object(Bucket=S3_BUCKET, Key=file_key)
                df = pd.read_csv(obj['Body'])
                
                # Define the target table name based on the file name (without the .csv extension)
                table_name = file_name[:-4]
                
                # Upload the DataFrame to the database
                df.to_sql(name=table_name, con=engine, if_exists='replace', index=False)
                
                print(f"Uploaded {file_name} to {table_name} in the database.")

if __name__ == '__main__':
    upload_files_from_s3_to_db()
