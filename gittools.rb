

module Git

  def self.pull(repository=nil)
    if repository.nil?
      raise 'Pull failed' unless system 'git pull'
    else
      raise 'Pull failed' unless system "git pull #{repository}"
    end
  end

  def self.clone(url, target:nil)
    if target.nil?
      raise 'Clone failed' unless system "git clone #{url}"
    else
      raise 'Clone failed' unless system "git clone #{url} #{target}"
    end
  end


end