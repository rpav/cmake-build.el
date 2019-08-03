cmake_minimum_required(VERSION 3.0)
project(mytest)

add_executable(mytest mytest.c)

add_custom_target(deploy
  COMMAND ${CMAKE_COMMAND} -E echo "If this were a real deployment, we would have done something."
  DEPENDS mytest
  )
