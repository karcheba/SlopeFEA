      MODULE TreeNodeDef

	IMPLICIT NONE

	TYPE TreeNode
	    CHARACTER(LEN=30) :: value
	    TYPE(TreeNode), POINTER :: left, right
	END TYPE TreeNode

	CONTAINS

	RECURSIVE SUBROUTINE AddNode(node, newVal)

	    IMPLICIT NONE
	    TYPE(TreeNode), POINTER :: node
	    CHARACTER(LEN=*), INTENT(IN) :: newVal

	    IF (.NOT.ASSOCIATED(node)) THEN
	        ALLOCATE(node)
	        node%value = newVal
	        NULLIFY(node%left)
	        NULLIFY(node%right)
	    ELSE IF (newVal < node%value) THEN
	        CALL AddNode(node%left, newVal)
	    ELSE
	        CALL AddNode(node%right, newVal)
	    END IF

      END SUBROUTINE AddNode


	RECURSIVE SUBROUTINE PrintNode(node)

	    IMPLICIT NONE
	    TYPE(TreeNode), POINTER :: node

	    IF (.NOT.ASSOCIATED(node)) RETURN

	    CALL PrintNode(node%left)
	    PRINT*, TRIM(node%value)
	    CALL PrintNode(node%right)

	END SUBROUTINE PrintNode

      END MODULE TreeNodeDef


	PROGRAM TreeSort

	    USE TreeNodeDef
	    
          IMPLICIT NONE
          TYPE(TreeNode), POINTER :: root
	    CHARACTER(LEN=30) :: newVal

	    NULLIFY(root)

	    PRINT*, "Enter character strings one at a time"
	    PRINT*, "to have them sorted alphabetically"
	    PRINT*, "<enter 'end' to exit data entry>:"

	    DO
	        READ*, newVal
	        IF (newVal .EQ. "end") EXIT
	        CALL AddNode(root, newVal)
	    END DO

	    PRINT*
	    PRINT*, "Here is the sorted list:"
	    CALL PrintNode(root)

	END PROGRAM TreeSort