# cogs/logging/log.scm
### **log!**
```scheme
(log! level arg-list)
```
Log directly on the specified level the with arguments, as a list
### **log/info!**
```scheme
(log/info! . args)
```
Log the arguments using the *info* target, i.e. log on INFO
### **log/warn!**
```scheme
(log/warn! . args)
```
Log the arguments using the *warn* target, i.e. log on WARN
### **log/debug!**
```scheme
(log/debug! . args)
```
Log the arguments using the *debug* target, i.e. log on DEBUG
### **log/error!**
```scheme
(log/error! . args)
```
Log the arguments using the *error* target, i.e. log on ERROR
### **log/trace!**
```scheme
(log/trace! . args)
```
Log the arguments using the *trace* target, i.e. log on TRACE
