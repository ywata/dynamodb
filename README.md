dynamodb
========

Haskell DynamoDB tools

# Current status of thie module
The author is currently deveoping this software. Most of the codes are intended to test DynamoDB API described in 
http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/Welcome.html.

# Preparing for DynamoDB local
As using dynamodb on AWS is not free but AWS released dynamodb test environment called DynamoDB 
in the following URL.
http://aws.typepad.com/aws/2013/09/dynamodb-local-for-desktop-development.html

As the released jar files does not provide us any information, the author modifies
the environment to get some error information.

1. enable log4j.rootLogger = ON in log4j.properties
2. add log4j.xml

This will help to understand the behaviour of DynamoDBLocal.
