# -*- coding: utf-8 -*-
"""
This fils includes some useful functins for python.
"""

#get the latest file name
def latest_filename(path):
    import os
    filemtime = []
    allfile = []
    for root,dirs,files in os.walk(path):
        for file in files:
            allfile.append(file)
            filemtime.append(os.path.getctime(os.path.join(root,file)))
            #print(os.path.getmtime(os.path.join(root,file))) 
            
    last_file_index = filemtime.index(max(filemtime))
    last_file = allfile[last_file_index]
    return(last_file)

#file check
def read_check_file(path, filename):
    f = open(path + filename, "r")  
    line = f.readlines()
    if line[0].lower() == 'success':
        return(True)
    else:
        return(False)

#SQL Connection
def sql_connect_file_read(tablename, col_list, server, database, uid, password):
    import pyodbc
    import pandas as pd
    sqlconnect = 'DRIVER={SQL Server};SERVER=' + server + '\\sql;DATABASE=' + database + '; UID=' + uid + '; PWD=' + password
    cnxn = pyodbc.connect(sqlconnect)
    cursor = cnxn.cursor()
    #SQL sentence
    sql = 'select ' + ','.join(col_list) + ' from ' + tablename
    raw_file = pd.DataFrame.from_records(cursor.execute(sql), columns =col_list)
    return(raw_file)

#Upload data to sql server using insert
def insert_tb_to_sqlserver(tablename, 
                           sql_table,
                           server, 
                           database, 
                           uid, 
                           password):
    import pyodbc
    sqlconnect = 'DRIVER={SQL Server};SERVER=' + server + '\\sql;DATABASE=' + database + '; UID=' + uid + '; PWD=' + password
    cnxn = pyodbc.connect(sqlconnect)
    cursor = cnxn.cursor()
    #SQL sentence
    s = "'%s'"
    for i in range(1,len(tablename.columns),1):
        s = s + ",'%s'"
    insertcolumn = "Insert into " + sql_table + " values(" + s + ")"
                                                        
    for rowid in range(1, len(tablename),1):
        line = tuple(tablename.iloc[rowid,:])
        insertcolumn_full = insertcolumn % line
        cursor.execute(insertcolumn_full)
        cnxn.commit()
    cnxn.close()
    return('True')

#SFTP Connection
##Using primary key file
def ssh_connect_key( _host, _port, _username, primary_key):
    import paramiko
    pkey=primary_key
    key=paramiko.RSAKey.from_private_key_file(pkey) 
    #paramiko.util.log_to_file('paramiko.log')
    ssh = paramiko.SSHClient()
    ssh.load_system_host_keys() 
    ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy()) 
    ssh.connect(hostname = _host,port = _port,username = _username,pkey=key) 
    return ssh

##Using Password
def ssh_connect_pwd( _host, _port, _username, _pwd):
    import paramiko
    ssh = paramiko.SSHClient()
    ssh.load_system_host_keys() 
    ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy()) 
    ssh.connect(hostname = _host,port = _port,username = _username, password = _pwd) 
    return ssh
 
##Upload files
def uploadfile(path, 
               sftp_folder, 
               filename,
               sftp_filename,
               hostname,
               port,
               username,
               password):
    #connect 
    sshd = ssh_connect_pwd(_host = hostname,\
                           _port = port,\
                           _username = username,\
                           _pwd = password)
    sftpd = sshd.open_sftp()
    #上传数据文件
    sftpd.put( path + filename, sftp_folder + sftp_filename )
    sftpd.close()
    sshd.close()

##Download files    
def downloadfile(path, 
                 sftp_folder, 
                 filename,
                 hostname,
                 port,
                 username,
                 password):
    #connect 
    sshd = ssh_connect_pwd(_host = hostname,\
                           _port = port,\
                           _username = username,\
                           _pwd = password)
    sftpd = sshd.open_sftp()
    #上传数据文件
    sftpd.get(sftp_folder + filename, path + filename )
    sftpd.close()
    sshd.close()

#Encode functin:MD5 or SHA256
def sha256(x):
    import hashlib as hash
    re = hash.sha256(x.strip().encode('utf-8')).hexdigest() if x != '' else ''
    return(re)

def md5(x):
    import hashlib as hash
    re = hash.md5(x.strip().encode('utf-8')).hexdigest() if  x != '' else ''
    return(re)

def col_encode(encoding_method, file, list_hash):
    if encoding_method.lower() == 'sha256':
        list_encode = [element + '_sha256' for element in list_hash]
        file[list_encode] = file[list_hash]\
                                .astype(str)\
                                .applymap(lambda x: sha256(x))
    elif encoding_method.lower() == 'md5':
        list_encode = [element + '_md5' for element in list_hash]
        file[list_encode] = file[list_hash]\
                               .astype(str)\
                               .applymap(lambda x: md5(x))
    return(file)
    
 
#计算整个文件的MD5或者SHA256值
def FileEncoded(encoding_method, path, filename):
    import hashlib as hash
    if encoding_method.lower() == 'sha256':
        myhash = hash.sha256()
    elif encoding_method.lower() == 'md5':
        myhash = hash.md5()
    f = open(path + filename,'rb')
    while True:
         b = f.read()
         if not b :
             break
         myhash.update(b)
    f.close()
    file_md5 = myhash.hexdigest()
    return(file_md5)

#输出txt文件
def output(file, path, filename, delimiter, index):
    import pandas as pd
    list = file.columns
    pd.DataFrame.to_csv(file,\
	                path + filename,\
	                sep = delimiter,\
	                header = list,\
	                index = index)

#ROC曲线和AUC
def roc(y_pred, response):
    from sklearn.metrics import roc_curve
    from sklearn.metrics import auc
    import matplotlib.pyplot as plt 
    fpr, tpr, thresholds = roc_curve(response.astype(int), y_pred.astype(float))
    #Calculate the AUC
    roc_auc = auc(fpr, tpr)  
    # Plot the ROC curve
    plt.plot(fpr, tpr, label='ROC curve (area = %0.3f)' % roc_auc)
    plt.plot([0, 1], [0, 1], 'k--')  # random predictions curve
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.0])
    plt.xlabel('False Positive Rate or (1 - Specifity)')
    plt.ylabel('True Positive Rate or (Sensitivity)')
    plt.title('Receiver Operating Characteristic')
    plt.legend(loc="lower right")
    return(roc_auc)

#P-R曲线和AUC	
def pr(y_pred, response):
    import numpy as np
    import matplotlib.pyplot as plt
    from sklearn.metrics import roc_curve
    from sklearn.metrics import precision_recall_curve, auc
    fpr, tpr, thresholds = roc_curve(response.astype(int), y_pred.astype(float))
    precision, recall, thresholds = precision_recall_curve(response, y_pred.astype(float))
    lab = 'ROC AUC = %0.3f' % (auc(fpr, tpr))
    ymax = round(np.mean(response) * 3, 3)
    plt.plot(recall,precision, label = lab)
    plt.xlabel('Recall')
    plt.ylabel('Precision')
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, ymax])
    plt.title('P-R Curve')
    plt.legend(loc = 'lower left')

#Precision and Recall Curve V.S. Thresholds
def plot_precision_recall_vs_threshold(precisions, recalls, thresholds):
    plt.plot(thresholds, precisions[:-1], "b--", label="Precision", linewidth=1)
    plt.plot(thresholds, recalls[:-1], "g-", label="Recall", linewidth=1)
    plt.xlabel("Threshold", fontsize=16)
    plt.legend(loc="upper left", fontsize=10)
    plt.ylim([0, 1])

#Calculate Lift Chart      
def lift_value(data, all_gp):
    import math
    import pandas as pd
    d2 = data.sort_values(by = ['Score'],axis = 0, ascending = False)
    cnt = math.ceil(len(d2)/all_gp)
    gpid = [math.floor(x / cnt) for x in range(len(d2))]
    d2['gpid'] = gpid
    functions=['count','sum','mean']
    gp_sub = d2[['response','gpid']].groupby(['gpid'])
    gp_lift = pd.DataFrame(gp_sub.agg(functions))['response']
    ovall_con = sum(d2['response'])/len(d2)
    gp_lift['lift'] = gp_lift['mean']/ovall_con
           
    s = [sum(gp_lift['sum'][0:(i + 1)]) for i in range(all_gp)]
    c = [sum(gp_lift['count'][0:(i + 1)]) for i in range(all_gp)]
    
    gp_lift['con_acc'] = [s[i]/(c[i]) for i in range(all_gp)]
    gp_lift['lift_acc'] = [s[i]/(c[i]*ovall_con) for i in range(all_gp)]  
    return(gp_lift)

def lift_chart(gp_lift, all_gp):
    import matplotlib.pyplot as plt
    x = gp_lift.index.ravel()
    y = gp_lift['lift_acc'].ravel()
    plt.plot(x, y)
    plt.xlabel('%d Groups' %(all_gp))
    plt.ylabel('Accumulate Lift')
    plt.title('Lift Chart')

#压缩文件: 7zip and rar zip
def zip_7z(zip_cmd, password, path, filename):
    import subprocess
    rc = subprocess.call([zip_cmd, 'a', '-tzip', '-p' + password, '-y', path + filename + '.zip']\
                         + [path + filename])
    re = True if rc == 0 else False
    return(re)

def zip_rar(path, filename):
    import zipfile
    zipf = zipfile.ZipFile(path + filename + '.zip','w',zipfile.ZIP_DEFLATED)
    zipf.write(path + filename + '.txt', './' + filename + '.txt')
    zipf.close()
    return(True)

def unzip(path, file, password):
    import zipfile
    pwd_str = password.encode("utf-8")
    rc = zipfile.ZipFile(path + file)
    rc.extractall(path = path, members=rc.namelist(), pwd=pwd_str)
    return(True)

#生成项目文件
def makdir(proj_path, batpath):
    import os
    import subprocess
    os.chdir(proj_path)
    p = subprocess.Popen("cmd.exe /c" + batpath + "/Make_Dirs.bat", 
                         stdout=subprocess.PIPE, 
                         stderr=subprocess.STDOUT)
    curline = p.stdout.readline()
    while(curline != b''):
        print(curline)
        curline = p.stdout.readline()
    p.wait()
    print(p.returncode)

#Download data file from URL and unzip it
def fetch_housing_data(housing_url=HOUSING_URL, housing_path=HOUSING_PATH):
    if not os.path.isdir(housing_path):
        os.makedirs(housing_path)
    tgz_path = os.path.join(housing_path, "housing.tgz")
    urllib.request.urlretrieve(housing_url, tgz_path)
    housing_tgz = tarfile.open(tgz_path)
    housing_tgz.extractall(path=housing_path)
    housing_tgz.close()

# 计算IV值
def IV(data,      #raw data
       q,         #number of group based on quantiles
       response,  #target variables 
       var):      #variable to be grouped
     import pandas as pd
     import numpy as np
     d = data[[response, var]]
     v = d[var].astype(float)
     cut_point = v.quantile([x/q for x in range(q+1)])
     v_gp = {var + '_gp': pd.cut(v, cut_point)}
     t_gp = pd.DataFrame(v_gp)
     t = pd.concat([data, t_gp], axis = 1)
     functions=['count','sum']
     t_sub = pd.DataFrame(t.groupby([var + '_gp']).agg(functions)[response])
     t_sub.columns = ['total_cnt','pos_cnt']
     t_sub['neg_cnt'] = t_sub['total_cnt'] - t_sub['pos_cnt']
     all_pos = sum(d[response])
     all_neg = len(d) - all_pos
     t_sub['p_pos'] = t_sub['pos_cnt']/all_pos
     t_sub['p_neg'] = t_sub['neg_cnt']/all_neg
     t_sub['WOE'] = np.log(t_sub['p_pos']/t_sub['p_neg'])
     t_sub['IV'] = (t_sub['p_pos'] - t_sub['p_neg'])*t_sub['WOE']
     t_sub = t_sub.drop(['total_cnt'], axis = 1)
     return(t_sub)





