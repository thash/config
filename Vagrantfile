Vagrant.configure(2) do |config|
  config.vm.define :zaim do |zaim|
    zaim.vm.box = 'zaim_ubuntu'
    # make sure /home/zaim/.ssh/authorized_keys not to be modified by chef-solo.
    zaim.ssh.username = 'zaim'
    # zaim.ssh.port = 2222

    zaim.vm.network :private_network, ip: '192.168.33.10'

    zaim.vm.synced_folder "output/", "/vagrant/output"

    zaim.vm.provider :virtualbox do |vb|
      # Use VBoxManage to customize the VM. For example to change memory:
      vb.customize ['modifyvm', :id, '--memory', 2048]
    end
  end
end
