Vagrant.configure("2") do |config|

  config.vm.define :debian do |debian|
    debian.vm.box = "debian7.2"
    debian.vm.network :private_network, ip: "192.168.33.29"
    debian.vm.provider :virtualbox do |vb|
      vb.customize ["modifyvm", :id, "--memory", "2048"]
    end
  end

  config.vm.define :zaim do |zaim|
    zaim.vm.box = "zaim_ubuntu"
    ### because run chef-solo and overwrite ~/.ssh/authorized_keys, cannot login as zaim directly.
    # zaim.ssh.username = "zaim"

    zaim.vm.network :private_network, ip: "192.168.33.10"

    zaim.vm.provider :virtualbox do |vb|
      # Use VBoxManage to customize the VM. For example to change memory:
      vb.customize ["modifyvm", :id, "--memory", "2048"]
    end
  end
end
