try{
 undefinedfunction()
 alert('I guess you do exist')
}
catch(e){
 alert('An error has occurred: '+e.message)
}
finally{
 alert('I am alerted regardless of the outcome above')
}