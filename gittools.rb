

module Git

  def pull(repository=nil)
    if repository.nil?
      `git pull`
    else
      `git pull #{repository}`
    end
  end

  def clone(url)
    `git clone #{url}`
  end


end