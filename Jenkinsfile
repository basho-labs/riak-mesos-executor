["ubuntu-14.04", "centos-7"].each {
  docker.image("basho/build-essential:$it").inside {
    checkout scm
    echo 'Starting build...'
  }
}
