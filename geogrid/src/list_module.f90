!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE LIST_MODULE
!
! Purpose: This module implements a list with insert, search, and
!   remove routines. 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module list_module

   type list_item
      integer :: ikey, ivalue
      character (len=128) :: ckey, cvalue
      type (list_item), pointer :: next, prev
   end type list_item
 
   type list
      integer :: l_len
      type (list_item), pointer :: head
   end type list

   contains
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_init
   !
   ! Purpose: To initialize a list type 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine list_init(l)
   
      implicit none
  
      ! Arguments
      type (list), intent(inout) :: l
  
      nullify(l%head)
      l%l_len = 0
    
   end subroutine list_init
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_insert
   !
   ! Purpose: Given a list l, a key, and a value to be stored with that key,
   !   this routine adds (key, value) to the table. 
   !
   ! NOTE: If the key already exists in the list, a second copy of a list item 
   !   with that key is added, possibly with a different associated value. 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine i_list_insert(l, key, value)
   
      implicit none
  
      ! Arguments
      integer, intent(in) :: key, value
      type (list), intent(inout) :: l
  
      ! Local variables
      type (list_item), pointer :: lp 
  
      allocate(lp)
      lp%ikey = key
      lp%ivalue = value
  
      lp%next => l%head
      if (associated(l%head)) l%head%prev => lp
      l%head => lp
  
      l%l_len = l%l_len + 1
 
   end subroutine i_list_insert
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_insert
   !
   ! Purpose: Given a list l, a key, and a value to be stored with that key,
   !   this routine adds (key, value) to the table. 
   !
   ! NOTE: If the key already exists in the list, a second copy of a list item 
   !   with that key is added, possibly with a different associated value. 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine c_list_insert(l, key, value)
   
      implicit none
  
      ! Arguments
      character (len=128), intent(in) :: key, value
      type (list), intent(inout) :: l
  
      ! Local variables
      type (list_item), pointer :: lp 
  
      allocate(lp)
      lp%ckey = key
      lp%cvalue = value
  
      lp%next => l%head
      if (associated(l%head)) l%head%prev => lp
      l%head => lp
  
      l%l_len = l%l_len + 1
 
   end subroutine c_list_insert

 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_get_keys
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   function list_get_keys(l)

      implicit none

      ! Arguments
      type (list), intent(in) :: l

      ! Return value
      type (list_item), pointer, dimension(:) :: list_get_keys

      ! Local variables
      integer :: i
      type (list_item), pointer :: lp 

      allocate(list_get_keys(l%l_len)) 

      lp => l%head
  
      i = 1
      do while (associated(lp))
         list_get_keys(i)%ikey   = lp%ikey
         list_get_keys(i)%ivalue = lp%ivalue
         list_get_keys(i)%ckey   = lp%ckey
         list_get_keys(i)%cvalue = lp%cvalue
         lp => lp%next
         i = i + 1
      end do

      return

   end function list_get_keys
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_search
   !
   ! Purpose: If key k is found in the list, this function returns TRUE and sets 
   !   value equal to the value stored with k. If the k is not found, this
   !   function returns FALSE, and value is undefined.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   function i_list_search(l, k, value)
   
      implicit none
  
      ! Arguments
      integer, intent(in) :: k
      integer, intent(out) :: value
      type (list), intent(inout) :: l
  
      ! Return value
      logical :: i_list_search
  
      ! Local variables
      type (list_item), pointer :: lp 
  
      i_list_search = .false.
  
      lp => l%head
  
      do while (associated(lp))
         if (lp%ikey == k) then
            i_list_search = .true.
            value = lp%ivalue
            exit
         end if
         lp => lp%next
      end do
 
   end function i_list_search
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_search
   !
   ! Purpose: If key k is found in the list, this function returns TRUE and sets 
   !   value equal to the value stored with k. If the k is not found, this
   !   function returns FALSE, and value is undefined.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   function c_list_search(l, k, value)
   
      implicit none
  
      ! Arguments
      character (len=128), intent(in) :: k
      character (len=128), intent(out) :: value
      type (list), intent(inout) :: l
  
      ! Return value
      logical :: c_list_search
  
      ! Local variables
      type (list_item), pointer :: lp 
  
      c_list_search = .false.
  
      lp => l%head
  
      do while (associated(lp))
         if (lp%ckey == k) then
            c_list_search = .true.
            value = lp%cvalue
            exit
         end if
         lp => lp%next
      end do
 
   end function c_list_search


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: list_get_first_item
   !
   ! Purpose: Sets k and v equal to the key and value, respectively, of the
   !   first item in the list. The "first item" should not be assumed to be
   !   related in any way to the order in which items were added or removed from
   !   the list; rather, the "first item" is simply the first item encountered
   !   in the internal storage of items. This item is also removed from the
   !   list before the subroutine returns.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine i_list_get_first_item(l, k, v)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: k, v
      type (list), intent(inout) :: l
 
      ! Local variables
      type (list_item), pointer :: lp
  
      lp => l%head
  
      if (associated(lp)) then
         k = lp%ikey
         v = lp%ivalue
         l%head => lp%next
         if (associated(lp%next)) nullify(lp%next%prev)
         deallocate(lp)
         l%l_len = l%l_len - 1
      end if
 
   end subroutine i_list_get_first_item
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: list_get_first_item
   !
   ! Purpose: Sets k and v equal to the key and value, respectively, of the
   !   first item in the list. The "first item" should not be assumed to be
   !   related in any way to the order in which items were added or removed from
   !   the list; rather, the "first item" is simply the first item encountered
   !   in the internal storage of items. This item is also removed from the
   !   list before the subroutine returns.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine c_list_get_first_item(l, k, v)
 
      implicit none
  
      ! Arguments
      character (len=128), intent(out) :: k, v
      type (list), intent(inout) :: l
 
      ! Local variables
      type (list_item), pointer :: lp
  
      lp => l%head
  
      if (associated(lp)) then
         k = lp%ckey
         v = lp%cvalue
         l%head => lp%next
         if (associated(lp%next)) nullify(lp%next%prev)
         deallocate(lp)
         l%l_len = l%l_len - 1
      end if
 
   end subroutine c_list_get_first_item
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_remove
   !
   ! Purpose: Deletes the entry with key k from the list. If multiple entries 
   !   have the specified key, only the first encountered entry is deleted.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine i_list_remove(l, k)
   
      implicit none
  
      ! Arguments
      integer, intent(in) :: k
      type (list), intent(inout) :: l
  
      ! Local variables
      type (list_item), pointer :: lp 
  
      lp => l%head
  
      do while (associated(lp))
         if (lp%ikey == k) then
    
            if (.not. associated(lp%prev)) then
               l%head => lp%next
               if (associated(lp%next)) nullify(lp%next%prev)
               deallocate(lp)
            else if (.not. associated(lp%next)) then
               nullify(lp%prev%next)
               deallocate(lp)
            else
               lp%prev%next => lp%next
               lp%next%prev => lp%prev
               deallocate(lp)
            end if
            l%l_len = l%l_len + 1
    
            exit
   
         end if
         lp => lp%next
      end do
 
   end subroutine i_list_remove
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_remove
   !
   ! Purpose: Deletes the entry with key k from the list. If multiple entries 
   !   have the specified key, only the first encountered entry is deleted.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine c_list_remove(l, k)
   
      implicit none
  
      ! Arguments
      character (len=128), intent(in) :: k
      type (list), intent(inout) :: l
  
      ! Local variables
      type (list_item), pointer :: lp 
  
      lp => l%head
  
      do while (associated(lp))
         if (lp%ckey == k) then
    
            if (.not. associated(lp%prev)) then
               l%head => lp%next
               if (associated(lp%next)) nullify(lp%next%prev)
               deallocate(lp)
            else if (.not. associated(lp%next)) then
               nullify(lp%prev%next)
               deallocate(lp)
            else
               lp%prev%next => lp%next
               lp%next%prev => lp%prev
               deallocate(lp)
            end if
            l%l_len = l%l_len + 1
    
            exit
   
         end if
         lp => lp%next
      end do
 
   end subroutine c_list_remove
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_length
   !
   ! Purpose: Returns the number of items in the list l.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   function list_length(l)
 
      implicit none
  
      ! Arguments
      type (list), intent(in) :: l
  
      ! Return value
      integer :: list_length
  
      list_length = l%l_len
  
      return
 
   end function list_length
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_destroy
   !
   ! Purpose: Frees all memory associated with list l. This routine may be
   !   used to remove all entries from a list.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine list_destroy(l)
   
      implicit none
  
      ! Arguments
      type (list), intent(inout) :: l
  
      ! Local variables
      type (list_item), pointer :: lp
  
      lp => l%head
  
      do while (associated(lp))
         l%head => lp%next
         deallocate(lp)
         lp => l%head
      end do
  
      l%l_len = 0
    
   end subroutine list_destroy
 
end module list_module
