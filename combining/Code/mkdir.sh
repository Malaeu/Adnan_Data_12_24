#!/bin/bash

# Создаем директории Code, Results и Data_Sets
mkdir -p Code Results Data_Sets

# Проверяем, что все создалось нормально
if [ $? -eq 0 ]; then
    echo "Директории успешно созданы, братан!"
else
    echo "Бля, что-то пошло не так. Проверь права доступа или свободное место."
fi
