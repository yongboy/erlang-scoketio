erlang-scoketio
===============

The mirroring of https://code.google.com/p/erlang-scoketio/

Another eralng socket.io server

1. socket.io server base on cowboy

    Checkout url: https://erlang-scoketio.googlecode.com/svn/socket.io_cowboy

    Now Compatible with socket.io-spec(https://github.com/LearnBoost/socket.io-spec) 0.9

    Support xhr-polling/jsonp-polling/htmlfile/websocket/flashsocket transports

    Support CJK language. 

2. socket.io server base on mochiweb

    Checkout url: https://erlang-scoketio.googlecode.com/svn/socket.io_mochiweb

    Now Compatible with socket.io-spec 1.0

    Only support xhr-polling/htmlfile/jsonp-polling

    Now stop update it. 

3. run one demo

    Go to http://code.google.com/p/erlang-scoketio/downloads/detail?name=socket.io.zip page

4. how to start your new project

    a. download the source, or svn checkout
    
    b.#chmod a+x
    
    c.#make app PROJECT=your project name PREFIX=you path here
    (eg: #make app PROJECT=chatdemo PREFIX=/home/yongboy/)
    
    d.entry your custom path, then #make
    
    e.#sh start.sh
    
    f. view http://yourip:8080/index.html
    
    enjoy it~

    Then download it step by step. 

If you want java version socket.io server, here is the project: http://code.google.com/p/socketio-netty/

Enjoy it~

Erlang 版本的socket.io服务器实现

1. 基于cowboy构建

    检出地址： https://erlang-scoketio.googlecode.com/svn/socket.io_cowboy

    兼容 socket.io-spec(https://github.com/LearnBoost/socket.io-spec) 1.0

    支持xhr-polling/jsonp-polling/htmlfile/websocket/flashsocket等通讯协议

    支持CJK语言，UTF-8编码下很少出现乱码

    现在可以作为0.1版本释出，具有一个chat示范

2. 基于mochiweb构建

    检出地址: https://erlang-scoketio.googlecode.com/svn/socket.io_mochiweb

    兼容 socket.io-spec(https://github.com/LearnBoost/socket.io-spec) 1.0

    仅支持xhr-polling/jsonp-polling/htmlfile等通讯协议

    暂时精力有限，停止更新，假若有需要，可以进一步有偿商谈

3. 运行一个示范

    到此下载示范文件http://code.google.com/p/erlang-scoketio/downloads/detail?name=socket.io.zip ，按照步骤执行即可。 

4. 如何开始

    a. 想法检出源代码
    
    b.linux下执行 #chmod a+x 赋予执行权限
    
    c.#make app PROJECT=工程名 PREFIX=保存路径
    (eg: #make app PROJECT=chatdemo PREFIX=/home/yongboy/)

    记得保存路径最好最好有一个/符号
    d.进入项目的保存路径，然后执行 #make 即可下载依赖、编译程序等
    e.启动服务器 #sh start.sh
    f. 然后访问 http://yourip:8080/index.html
    希望您会喜欢

若您需要Java版本socket.io服务器实现，可参考：http://code.google.com/p/socketio-netty/ 
