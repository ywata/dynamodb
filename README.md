dynamodb
========

Haskell DynamoDB tools

# Current status of thie module
The author is currently deveoping this software. Most of the codes are intended to test DynamoDB API described in 
http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/Welcome.html.

Current status of the implementation

Table Manupilacion API.
* CreateTable
* DeleteTable
* DescribeTable
* ListTables
* UpdateTable

Item Manupilation API.
* PutItem
* GetItem
* DeleteItem
* UpdateItem

Search API.
* Query
* Scan

Batch API is not urrently implemented.
* BatchGetItem
* BatchWriteItem

# Preparing for DynamoDB local
As using dynamodb on AWS is not free but AWS released dynamodb test environment called DynamoDBLocal
in the following URL.
http://aws.typepad.com/aws/2013/09/dynamodb-local-for-desktop-development.html

As the released jar files does not provide us any information for debug, the author modifies
the environment to get some error information.

* enable log4j.rootLogger = ON in log4j.properties
* add log4j.xml

This will help to understand the behaviour of DynamoDBLocal.

The author created a script to downlod and patch the above changes.
Go to util directory and run setupDynamoDBLocal.sh in the directory will
prepare dynamodb local.

Another setup necessary to use DynamoDBLocal is to put dummy aws-keys file on your home directory as .aws-keys 
A sample dummy aws-keys file is in the util directory.




