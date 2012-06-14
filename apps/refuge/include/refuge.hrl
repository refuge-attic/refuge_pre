-record(node, {
        name,
        host,
        port,
        time}).

-record(couchapp, {
        path,
        ignore = [],
        att_dir,
        docid,
        doc,
        old_doc=nil,
        manifest=[],
        attachments=[],
        signatures=[]
}).

-define(DEFAULT_IGNORE,[<<"^\\.git*">>, <<"\\.DS_Store">>,
        <<"\\.swp$">>, <<"\\.swo$">>, <<"~$">>, <<"\\.svn$">>,
        <<"\\.CVS$">>, <<"\\.CVS$">>, <<"^\\.hg$">>, <<"\\.#">>]).
