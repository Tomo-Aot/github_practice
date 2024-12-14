# 2024-12-10
# very simple application in ubuntu
# how to install tkinter in ubuntu24.04LTS
# open terminal and do the following command
# sudo apt-get install python3-tk
import tkinter as tk

# メインウィンドウの作成（tk.TK()）
root = tk.Tk()

# Rootは、ブラウザのウィンドウを指します。
# root.title() でタイトルを決めます。
# このタイトルは、ウィンドウの上部に表示するものです。
# デフォルトはtkです。
root.title("GUI App Workspace")

# テキストを設定します。
# \n を入れると、テキストを改行することができます。
text = "Hello World!\nThis is my python workspace!"\
        "in this script, i left good codes"

# フォントはNoto Sans、サイズは16ptにします。
font = ("Noto Sans", 16, "bold")

# label は、テキストをウィンドウに表示するためのもの
# 先程設定したタイトルとテキストをインプット
label = tk.Label(root, text = text,
                 font = font)

label.pack()

root.mainloop()
