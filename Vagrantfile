$script = <<-SCRIPT
set -eux

systemctl daemon-reload
apt update
apt install --yes tree mergerfs

if ! which just
then
  curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh | bash -s -- --to /usr/local/bin
fi
just --version

if ! which stack
then
  wget -qO- https://get.haskellstack.org/ | sh
fi
stack --version

cd /vagrant
su vagrant -c "stack install ghcid"
su vagrant -c "stack build --only-dependencies --test"
SCRIPT

Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/kinetic64"
  config.vm.provision "shell", inline: $script

  config.vm.provider "virtualbox" do |v|
    v.memory = 20000
    v.cpus = 16
  end
end
