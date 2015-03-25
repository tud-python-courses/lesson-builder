import logging
import os

__author__ = 'Justus Adam'
__version__ = '0.1'


DEBUG = True


CONFIG_NAME = 'build_conf.json'
BUILD_TIMEOUT = 2 * 60  # seconds

logging.basicConfig(
    format='[%(levelname)10s]:%(message)s',
    filename=os.path.join(os.path.dirname(__file__),'builder.log')
)

ERROR_LOG_FILE = 'builder-error.log'
INFO_LOG_FILE = 'builder.log'