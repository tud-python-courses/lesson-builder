

module Git

  def pull(repository=nil)
    if repository.nil?
      raise 'Pull failed' unless system 'git pull'
    else
      raise 'Pull failed' unless system "git pull #{repository}"
    end
  end

  def clone(url)
    raise 'Clone failed' unless "git clone #{url}"
  end


end