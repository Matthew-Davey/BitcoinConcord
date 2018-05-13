# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/trusty64"
  config.vm.network "forwarded_port", guest: 5984, host: 5984
  config.vm.network "forwarded_port", guest: 5672, host: 5672
  config.vm.network "forwarded_port", guest: 15672, host: 15672
  config.vm.provision "docker" do |docker|
    docker.run "couchdb", image: "couchdb:2.1.1", args: "-p 5984:5984"
  end
  config.vm.provision "docker" do |docker|
    docker.run "rabbitmq", image: "rabbitmq:3.7.4-rc.3-management-alpine", args: "-p 5672:5672 -p 15672:15672"
  end
end
