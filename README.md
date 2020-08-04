### recordis 

a redis orm

#### 本项目用于解决以下问题

    1. redis储存导致键的依赖关系混乱，使用者无法直观地看到键的类型，键与键的关联关系。
    导致删除，修改的时候非常容易出错或遗忘，
    2. 拓展数据类型，比如redis中不区分int和string，每次从redis中取数据时，都要进行类型转换。
    3. 部分键用erlang:term_binary()这种方式去存，导致查询和修改都很不方便。
    4. 如果要增加一些搜索功能，会增加很多代码，数据也容易不同步。
   
#### 对于record的格式约束过大的问题。

    1. 可以通过parse_transfer实现，但我觉得意义不大。
    2. 通过函数校验即可保证正确性。
    
#### 使用了进程字典
    
    所以不要操作以下键 recordis，recordis_pipe

#### 锁

